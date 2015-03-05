# Advanced Output Variable Reporting

Files for the following exercise can be found in the ExampleFiles\\AdvancedOutput folder in your installation. A four page instruction sheet is included.

A shortened, bulleted list of steps is shown:

- run the existing input file to generate a list of the report variables available for your simulations.
- add report variables at various time aggregations to the file and run the simulation again.
- create a .RVI file to extract specific data at various time aggregations. 

Read more about obtaining custom output files (.CSV) using .RVI (Report Variable Input) files from the output in the InputOutputReference.pdf, subject: Using ReadVarsESO.

Simply said, an .RVI is a text file with a list of report variables that you want reported in a .CSV. You can easily develop multiple .RVI files which create different types of .CSV files. For example, separate .CSVs for only the exterior environment data or for only equipment energy consumption. MVI files are the equivalent kind of files for meter only output files (the .mtr files). Both .RVI and .MVI files follow this structure:

~~~~~~~~~~~~~~~~~~~~

    eplusout.eso   ! name of input eso file
    eplusout.csv   ! name of target csv file (or .tab)
    <variable name 1>
    <variable name 2>
     …
    <variable name n>
    0
~~~~~~~~~~~~~~~~~~~~

The first two lines are the default output file .ESO and the default .CSV filename. This is followed by a list of report variables, with the last line containing a 0.

1Run the ExerciseOutput1.IDF file.

2Open ExerciseOutput1.RDD and select at least 10 loads-related variables. *Note in ExerciseOutput1.IDF, the object "Output:VariableDictionary, idf;" writes the RDD output file as complete objects which can be pasted directly into the IDF file and then edit the reporting frequency.*

Edit ExerciseOutput1.IDF using the text editor, and save as ExerciseOutput1A.IDF. Paste output:variable objects for each of your loads-related variables requesting hourly data. Then copy each object and paste in 4 copies for a total of 5. Then edit the frequency parameter on each, changing "hourly" to timestep, daily, monthly, and annual, retaining hourly for one of them. There are already system related output variables with multiple reporting frequencies in the .idf file that you can use as a model. For example, Zone Window Heat Gain and Zone Window Heat Loss, insert these objects in your IDF to get data at each of these time steps:

~~~~~~~~~~~~~~~~~~~~

    Output:Variable, *, Zone Window Heat Gain, timestep;
    Output:Variable, *, Zone Window Heat Gain, hourly;
    Output:Variable, *, Zone Window Heat Gain, daily;
    Output:Variable, *, Zone Window Heat Gain, monthly;
    Output:Variable, *, Zone Window Heat Gain, annual;
    Output:Variable, *, Zone Window Heat Loss, timestep;
    Output:Variable, *, Zone Window Heat Loss, hourly;
    Output:Variable, *, Zone Window Heat Loss, daily;
    Output:Variable, *, Zone Window Heat Loss, monthly;
    Output:Variable, *, Zone Window Heat Loss, annual;
~~~~~~~~~~~~~~~~~~~~

*Note that this step may also be done using IDF Editor. When an RDD file is present, the Output:Variable object will have an active drop-down list showing all of the report variable names present in the RDD output file.*

- Run the ExerciseOutput1A.IDF file. 
- Using your text editor, open ExerciseOutput1A.idf. Open a new file, and save it as ExerciseOutput1A-LOADS.RVI. Type in the following:

~~~~~~~~~~~~~~~~~~~~

    eplusout.eso
    eplusout.csv
~~~~~~~~~~~~~~~~~~~~

In the .idf file, locate the Output:Variable commands you just added. Copy them, and paste them into the new .RVI file. Delete the duplicates with different reporting frequencies, saving one instance of each variable. Delete everything but the variable name. Add a final line containing only a 0 (zero). For Window Heat Loss and Heat Gain, the .RVI file would look like this:

~~~~~~~~~~~~~~~~~~~~

    eplusout.eso
    eplusout.csv
    Zone Window Heat Gain
    Zone Window Heat Loss
    0
~~~~~~~~~~~~~~~~~~~~

- Rename file "ExerciseOutput1-CustomCSV.b~t" to "ExerciseOutput1¬CustomCSV.bat" and edit this file in a text editor to make sure the path at the top of the file matches where your version of EnergyPlus is installed. The current path in the file is: 

- set post_proc=C:\\EnergyPlusV6-0-0\\PostProcess\\ 

- Open a Command Window (Start, Run, Command) 
- Change to the directory containing your ExerciseOutput1A.IDF, results files, and your new ExerciseOutput1A-LOADS.RVI. For example: 

- CD D:\\EnergyPlus Training\\EnergyPlusExercises substitute your path here 

Note: This assumes that the ExerciseOutput1-CustomCSV.bat file is located in the same directory as your IDF and RVI. This is what EP-Launch does for single simulations.

- Type: ExerciseOutput1-CustomCSV ExerciseOutput1A ExerciseOutput1A-LOADS and press Enter. That is,

~~~~~~~~~~~~~~~~~~~~

    ExerciseOutput1-CustomCSV ExerciseOutput1A ExerciseOutput1A-LOADS
~~~~~~~~~~~~~~~~~~~~

- ExerciseOutput1-CustomCSV reads the ESO output and creates a .CSV for the .RVI for only the variables listed in the .RVI. A .CSV is created for each of the time steps in the output file--timestep, hourly, daily, monthly, or runperiod: inputfilename_timestep.csv, or for this exercise, ExerciseOutput1A.idf: 

~~~~~~~~~~~~~~~~~~~~

    ExerciseOutput1A_timestep.csv
    ExerciseOutput1A_hourly.csv
    ExerciseOutput1A_daily.csv
    ExerciseOutput1A_monthly.csv
    ExerciseOutput1A_annual.csv
~~~~~~~~~~~~~~~~~~~~

*If there is no data at the requested time step, that .CSV file will be empty, although that should not occur here.*

- Add report variables to the IDF for energy end-uses. Review .RDD, .MDD and .MTR file for variables to include. Open and save ExerciseOutput1A.idf as ExerciseOutput1B.idf. Create an energy end-use .MVI using the same structure as above but replace eplusout.eso with eplusout.mtr in the first line. Rerun the new IDF and run ExerciseOutput1-CustomCSV again: 

~~~~~~~~~~~~~~~~~~~~

    ExerciseOutput1-CustomCSV ExerciseOutput1B ExerciseOutput1B-ENERGYENDUSE
~~~~~~~~~~~~~~~~~~~~

- Experiment with creating other .RVIs and variables. Example .RVIs for ExerciseOutput1-EquipmentConsumption and ExerciseOutput1-ExternalEnvironment are included.