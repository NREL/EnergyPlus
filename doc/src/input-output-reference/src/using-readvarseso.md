# Using ReadVarsESO

## Creating Charts and Spreadsheet files from Output Variables

The ReadVarsESO program is distributed with EnergyPlus as a simple approach to converting the standard output files (**eplusout.eso, eplusout.mtr**) into files that can be put directly into a spreadsheet program and then used to create graphs or do other statistical operations. ReadVarsESO can read the complex output files and sort the data into time-based form, it is a very quick application but does not have a lot of features.  Note that all the **Output:Meter** and **Output:Meter:Cumulative** objects are included on the **eplusout.eso** file as well as the **eplusout.mtr** file.  You can choose the **Output:Meter:MeterFileOnly** or **Output:Meter:Cumulative:MeterFileOnly** objects if you do not want a particular meter to show up on the **eplusout.eso** file. If you wish to see only the metered outputs, the **eplusout.mtr** file will typically be a lot smaller than the **eplusout.eso** file.

The ReadVarsESO program has a very simple set of inputs. By default, you will get all the variables listed in the [Output:Variable](#outputvariable) (**eplusout.eso**) or [Output:Meter](#outputmeter-and-outputmetermeterfileonly) (**eplusout.mtr**) objects into the appropriate output files. The outputs from ReadVarsESO are limited to 255 variables (Microsoft Excel™ limit). You can tailor how many variables to list by specifying variables for the ReadVarsESO runs.

You can override the 255 variable limit by specifying an argument on the command line (**EP-Launch** has a special option for this). You use "unlimited" or "nolimit" on the command line to get as many variables into your output file as desired. Again, this will be limited either by the number of variables in the **eplusout.eso** or **eplusout.mtr** files or the contents of the "rvi" file. If you want to use this option, you must include two arguments to the command line of the ReadVars program – 1) the "rvi" file and 2) the "unlimited" argument.

Table: ReadVarsESO Command Line Options

Option|Description
------|-----------
<filename>|To use any of these options, you must include a file name ("rvi" file) as the first argument.
Unlimited (or Nolimit)|Variables of any number will be produced on the output file.
Timestep|Only variables with reported frequency "timestep" (or detailed) will be produced on the output file.
Hourly|Only variables with reported frequency "hourly"  will be produced on the output file.
Daily|Only variables with reported frequency "daily"  will be produced on the output file.
Monthly|Only variables with reported frequency "monthly"  will be produced on the output file.
Annual (or RunPeriod)|Only variables with reported frequency "runperiod"  will be produced on the output file.

In addition, another argument can be used so that the output file is only one time frequency. (By default, all variables – hourly, monthly, annual, etc. are mixed together in the output file). By using "[Timestep](#timestep)" as an argument, you would get only the TimeStep reported variables. Using "Monthly", only the monthly variables. This is not automated in either **EP-Launch** or the **RunEPlus** batch file but can easily be accomplished.

The program is run automatically from the **EP-Launch** program or the **RunEPlus** batch file (both these methods for executing EnergyPlus are described in the GettingStarted document). These programs use **<filename>.rvi** for input to the ReadVarsESO program executed first after the EnergyPlus execution and **<filename>.mvi** for the second execution. Ostensibly, the .rvi file would apply to the eplusout.eso file and the .mvi would apply to the eplusout.mtr file, BUT the contents of the files actually specify the "**input**" reporting file and the "**output**" reorganized file. Typical contents of an .rvi file are:

Table: "RVI" file contents

.rvi line description|Actual .rvi File Contents
---------------------|-------------------------
Input File|eplusout.eso
Output File|eplusout.csv
Variable Name|Site Outdoor Drybulb Temperature
Variable Name|Zone Air Temperature
Variable Name|Zone Air Humidity Ratio
Variable Name|Zone Air System Sensible Cooling Rate
Variable Name|Zone Air System Sensible Heating Rate
Variable Name|Zone Total Internal Latent Gain Rate
Specific Variable Name|COOLING COIL AIR OUTLET NODE,System Node Temperature
Specific Variable Name|AIR LOOP OUTLET NODE,System Node Temperature
Specific Variable Name|AIR LOOP OUTLET NODE,System Node Humidity Ratio
Specific Variable Name|Mixed Air Node,System Node Mass Flow Rate
Specific Variable Name|Outdoor air Inlet Node,System Node Mass Flow Rate
Variable Name|Humidifier Water Consumption Rate
Variable Name|Humidifier Electric Power
Variable Name|Zone Air Relative Humidity
Variable Name|Zone Predicted Moisture Load Moisture Transfer Rate
Termination Line (optional)|0

Note that the first two lines of the file are "input file" (where to read the output variable values from) and "output file" (where to put the reorganized data). If you have only those two lines in an "rvi" file, the program will use all the available variables on indicated input file to produce the output.

ReadVarsESO takes the input stream but recognizes the date/time information and appropriately places the required data onto the "output file". Following these lines are a list of variables to be culled from the "input file" and placed onto the output file. "Variable Name" will take all variables matching that description whereas "Specific Variable Name" will only match the full description of the variable. So, in the above example, "[Zone](#zone) Air Temperature" will report air temperatures for all the zones (but available at the HVAC timestep if you choose the "detailed" reporting frequency in your input file) but "AIR LOOP OUTLET NODE" and "COOLING COIL AIR OUTLET NODE" will be the only values reported for the "System Node Temperature" variable (the node temperature is available for all nodes used in the simulation). The termination line (0) is included to terminate the input to the ReadVarsESO program and begin the scanning.

The output from ReadVarsESO is in the form commonly called "comma de-limited" or "comma separated variable". This format can be read easily in spreadsheet programs, such as Excel™.

Note as described in the "Input for Output" above, only variables as listed on the **eplusout.rdd** file are available for reporting. If you request others, they will become "Warning" messages in the **eplusout.err** file.

~~~~~~~~~~~~~~~~~~~~

    ** Warning ** The following Output variables were requested but not generated
    **   ~~~   ** because IDF did not contain these elements or misspelled variable name -- check .rdd file
    ************* Key=*, VarName=SYSTEM SENSIBLE COOLING RATE
~~~~~~~~~~~~~~~~~~~~

The above message was generated from an IDF that requested reporting of the "SYSTEM SENSIBLE COOLING RATE" but that variable was not available from the components in the execution.

Table: Example ReadVarsESO command lines and results

Command Line|Description/Result
------------|------------------
ReadVarsESO|Take eplusout.eso and produce an eplusout.csv file with all variables (up to 255) on it
ReadVarsESO my.rvi|Use the contents of "my.rvi" to produce the appropriate output file (limited to 255 variables)
ReadVarsESO my.rvi unlimited|Use the contents of "my.rvi" to produce the appropriate output file (no longer limited to 255 variables)
ReadVarsESO my.rvi Monthly|Use the contents of "my.rvi" to produce the appropriate output file and only produce those variables reported for "monthly" frequency (up to 255 variables)
ReadVarsESO my.rvi Daily unlimited|Use the contents of "my.rvi" to produce the appropriate output file and only produce those variables reported for "daily" frequency (no longer limited to 255 variables)