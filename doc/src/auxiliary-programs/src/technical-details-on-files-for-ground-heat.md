# Technical Details on Files for Ground Heat Transfer with Slabs

The slab program used to calculate the results is included with the EnergyPlus distribution. It requires an input file named GHTin.idf in input data file format. The needed corresponding idd file is SlabGHT.idd. An EnergyPlus weather file for the location is also needed. A batch file, RunSlab, is placed in the same folder with the program and sample files. To use it (Command mode or DOS mode), one may modify several important lines:

~~~~~~~~~~~~~~~~~~~~
    :Instructions:
    :  Complete the following path and program names.
    :  path names must have a following \ or errors will happen
     set program_path=
     set program_name=Slab.exe
     set input_path=
     set output_path=
     set weather_path=C:\EnergyPlus\WeatherData\
~~~~~~~~~~~~~~~~~~~~

and then in command mode issue the run command:

~~~~~~~~~~~~~~~~~~~~
    RunSlab myinput Chicago
~~~~~~~~~~~~~~~~~~~~

Where you would have myinput.idf in "input_path" and Chicago would be the name of the .epw file in the "weather_path".

You should set up the command mode so that it does not automatically close the window at the end of program termination if you want to see the commands as they run and know for sure that no errors occurred.
