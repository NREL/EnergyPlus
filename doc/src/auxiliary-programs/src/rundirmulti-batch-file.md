# RunDirMulti Batch File

The RunDirMulti.bat batch file runs all the IDF files in the directory that it is located in. The batch file is used to run EnergyPlus simulations using the RunEPlus.bat for all the files in the current directory across multiple separate processor cores.  It has  two parameters, the weather file name to use for simulations and the number of processors.

~~~~~~~~~~~~~~~~~~~~
    RunDirMulti <weather file> (opt) <number processor cores> (opt)
~~~~~~~~~~~~~~~~~~~~

The RunDirMulti batch file loops through the files located in the current directory and puts RunEPlus calls to each file into as many temporary batch files as processor cores and then starts each of the batch files. No load balancing between the cores is achieved using this method. The RunDirMulti.bat file should be located in a directory that contains the IDF files. Editing of the file may be necessary to match the directory that EnergyPlus is installed in.

Since the batch file starts up other batch files in different CMD windows, the only way to know that the simulations are all complete is when all of the other CMD windows are closed. Those windows are named "Batch Simulation\<n\>" where n is 1 to the number of processor cores you selected to use.
