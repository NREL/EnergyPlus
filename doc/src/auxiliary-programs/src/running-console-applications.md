# Running Console Applications

Several of the auxiliary programs included with EnergyPlus are Console Applications. This designation means that they are executed from the "command window" (Windows OS) or terminal window (Linux OS). We will include generic instructions for the Windows OS use of these applications in this section. Each program will also include specifics for the Windows OS in the individual program documentation.

As installed, the batch files that accompany console applications will be set so that file extensions are not included for input parameters, paths to installed data (such as weather data) will be set automatically, and these paths can be modified by the user as desired. (Instructions included a bit later in this section).

Generally, the steps for executing a console application is:

#. Open a DOS command prompt window (Start  Programs  Accessories  Command Prompt)
#. Change to the directory where EnergyPlus is installed (modify the commands below if you did not install EnergyPlus in the default install path):

~~~~~~~~~~~~~~~~~~~~
    C:
    CD \<root folder>\
~~~~~~~~~~~~~~~~~~~~

#. Change to the specific folder for the console application:

~~~~~~~~~~~~~~~~~~~~
    CD <folder>
~~~~~~~~~~~~~~~~~~~~

#. Run the program:

~~~~~~~~~~~~~~~~~~~~
    <batchfile> <input parameters>
~~~~~~~~~~~~~~~~~~~~
