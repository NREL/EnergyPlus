# Running the Program

EP-Launch can be used to run the CalcSoilSurfTemp program. CalcSoilSurfTemp is one of the options on the Utilities tab in EP-Launch. See the EP-Launch section in this document for more information on how to use EP-Launch with the CalcSoilSurfTemp program.

You can also run the CalcSoilSurfTemp program as a console application with a batch file RunCalcSoilSurfTemp. The argument to the batch file is the name of the weather file to be used. Steps to running this program follow:

#. Open a DOS command prompt window (Start  Programs  Accessories  Command Prompt)
#. Change to the directory where EnergyPlus is installed (modify the commands below if you did not install EnergyPlus in the default install path):

~~~~~~~~~~~~~~~~~~~~
    C:
    CD \<root folder>\
~~~~~~~~~~~~~~~~~~~~

#. Change to the calculate surface soil temperature folder:

~~~~~~~~~~~~~~~~~~~~
    CD PreProcess\CalcSoilSurfTemp
~~~~~~~~~~~~~~~~~~~~

#. Run the program:

~~~~~~~~~~~~~~~~~~~~
    runcalcsoilsurftemp USA_IL_Chicago-OHare_TMY2
~~~~~~~~~~~~~~~~~~~~

When installed, the batch file automatically points the path for weather data to the installed weather data path. If you save your weather files in a different location, you will need to modify the batch file or create your own with your own folder paths.

Note that the program is interactive (expects user responses) from the command line. These inputs are described in the following section.