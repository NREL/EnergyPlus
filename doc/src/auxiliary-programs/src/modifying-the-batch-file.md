# Modifying the batch file

Generally, the batch files set several environment variables that govern the execution of the specific program.

Table: Environment Variables used in Batch Files

**Environment Variables**
--------------------------------------
*Environment Variable Name*|*Description*
Program Path|Specific or relative program path
Program Name|Name of program
Input_Path|Input Path Specification
Output_Path|Output Path Specification
Weather_Path|Weather Data Path Specification

Or, as seen in the batch file text:

~~~~~~~~~~~~~~~~~~~~
    :Instructions:
    :  Complete the following path and program names.
    :  path names must have a following \ or errors will happen
     set program_path=
     set program_name=<specific program name will be here>
     set input_path=
     set output_path=
     set weather_path=..\..\WeatherData\
~~~~~~~~~~~~~~~~~~~~

As the instructions in the batch file show, the path character must terminate the path specification or errors can occur. The "weather_path" specification shows an example of using a "relative" path specification. Depending on the program, this specification, of course, might change.

~~~~~~~~~~~~~~~~~~~~
    set weather_path=..\..\WeatherData\
~~~~~~~~~~~~~~~~~~~~

What the specification says is that above (..) and above again (..) and then "WeatherData" is where the weather data files are located. This kind of relative path would be true for most "Preprocess" programs in the installed folders. The following illustrates the folder tree:

\<Root Folder\> (this is usually EnergyPlusV\<version\>)

Preprocess

Specific Program Folders

WeatherData

Thus, the user can simply put the name of the weather data file onto the batch file run and it will look for that file in the installed WeatherData folder.
