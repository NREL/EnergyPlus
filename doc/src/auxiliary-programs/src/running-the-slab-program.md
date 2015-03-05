# Running the Slab Program

EP-Launch can be used to run the Slab program using two different methods.

If the Slab objects (see Description of the Objects in the E+SlabGHT.IDD) are located in the standard EnergyPlus IDF input file than the Single Input File and Group of Input File tabs of EP-Launch can be used and the Slab preprocessor will be called automatically during the simulation process. In this case the Slab objects should all appear with the object name starting with "**GroundHeatTransfer:Slab:**" This option also requires a **GroundHeatTransfer:Control** object in the EnergyPlus idf file (see Input Output Reference).

If the Slab objects are located in a separate file, they should be run using the Slab option on the Utilities tab in EP-Launch. See the EP-Launch section in this document for more information on how to use EP-Launch with the Slab program.

You can also run the slab program as a console application.  To execute it, we have provided the batch file:

~~~~~~~~~~~~~~~~~~~~
    RunSlab
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
    RunSlab SlabExample USA_IL_Chicago-OHare_TMY2
~~~~~~~~~~~~~~~~~~~~

In the following section, some description of modifying the batch file and details on files that the slab program uses are given.
