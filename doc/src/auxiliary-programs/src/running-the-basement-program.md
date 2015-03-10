# Running the Basement Program

EP-Launch can be used to run the Basement program.

If the Basement Objects (see The Basement idd below) are located in the standard EnergyPlus IDF input file than the Single Input File and Group of Input File tabs of EP-Launch can be used and the Basement preprocessor will be called automatically during the simulation process. In this case the Basement objects should all appear with the object name starting with "**GroundHeatTransfer:Basement:**" This option also requires a **GroundHeatTransfer:Control** object in the EnergyPlus idf file (see Input Output Reference).

If the Basement objects are located in a separate file, they should be run using the Basement option on the Utilities tab in EP-Launch. See the EP-Launch section in this document for more information on how to use EP-Launch with the Basement program.

You can also run the Basement program as a console application. To execute it, we have provided the batch file:

~~~~~~~~~~~~~~~~~~~~
    RunBasement
~~~~~~~~~~~~~~~~~~~~

Steps for running the program

#. Open a DOS command prompt window (Start  Programs  Accessories  Command Prompt)
#. Change to the directory where EnergyPlus is installed (modify the commands below if you did not install EnergyPlus in the default install path):

~~~~~~~~~~~~~~~~~~~~
    C:
    CD \<rootfolder>\
~~~~~~~~~~~~~~~~~~~~

#. Change to the specific folder for the console application:

~~~~~~~~~~~~~~~~~~~~
    CD PreProcess\GrndTempCalc
~~~~~~~~~~~~~~~~~~~~

#. Run the program (for example use the example included with the install):

~~~~~~~~~~~~~~~~~~~~
    RunBasement BasementExample USA_IL_Chicago-OHare_TMY2
~~~~~~~~~~~~~~~~~~~~

In the following section, some description of modifying the batch file and details on files that the basement program uses are given.