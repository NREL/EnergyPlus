Running EnergyPlus
==================

EnergyPlus is written in C++, using many C++11 features. It runs as a console application on Windows, Mac, and Linux (tested on latest Ubuntu LTS). Details on running EnergyPlus are available in a separate document (Running EnergyPlus in Auxiliary Programs). The following files are needed to run EnergyPlus:

- EnergyPlus.exe (the executable file)
- Energy+.ini (described below)
- Energy+.idd (the input data dictionary file)
- in.idf (the input file)
- in.epw – optional (weather data file)

The input data dictionary and input data file have been discussed in the previous sections of this document.

For weather simulations, EnergyPlus accepts EnergyPlus weather files. The actual file name is **in.epw**.

The usual release for Windows platform also includes a dependence on a shared library, EnergyPlusAPI.dll (.so on Linux).

Energy+.ini
-----------

The Energy+.ini file is read by the EnergyPlus.exe program and is *very important* to the simulation run. In specific, this file contains a pointer to where the Energy+.idd file will be found. More description of the Energy+.ini contents is contained in the Auxiliary Programs document.

For the advanced user, there is also the “EPMacro” program, described in the Auxiliary programs document. You run it as a separate program before EnergyPlus (the batch file included in the install and shown in the GettingStarted document contains the commands). It is automatically run if you use the EP-Launch program.

EnergyPlus may create several output files. Whether some files are produced depends on the actual simulation (for example, the .rdd file is only produced when you request a Variable Dictionary using the Output:Report object). Similarly, the .dxf file is only produced when a DXF surface report is requested. Common output files are shown in the following table. A complete, up to date list is shown in the “Output Details and Examples Document”.

Table 47. EnergyPlus Output Files

<table class="table table-striped">
<tr>
<th>FileName</th>
<th>Description</th>
</tr>
<tr>
<td>Eplusout.audit</td>
<td>Echo of input</td>
</tr>
<tr>
<td>Eplusout.err</td>
<td>Error file</td>
</tr>
<tr>
<td>Eplusout.eso</td>
<td>Standard Output File (contains results from both Output:Variable and Output:Meter objects).</td>
</tr>
<tr>
<td>Eplusout.eio</td>
<td>One time output file</td>
</tr>
<tr>
<td>Eplusout.rdd</td>
<td>Output variable Data Dictionary</td>
</tr>
<tr>
<td>Eplusout.dxf</td>
<td>DXF (from Output:Report,Surfaces,DXF;)</td>
</tr>
<tr>
<td>Eplusout.mtr</td>
<td>Similar to .eso but only has Meter outputs.</td>
</tr>
<tr>
<td>Eplusout.mtd</td>
<td>Meter details report – what variables are on what meters and vice versa.</td>
</tr>
<tr>
<td>Eplusout.end</td>
<td>A one line summary of success or failure (useful for Interface programs)</td>
</tr>
<tr>
<td>Eplusout.dbg</td>
<td>From Debug Output object – may be useful to support to help track down problems</td>
</tr>
<tr>
<td>Eplusout.&lt;ext&gt;</td>
<td>Not directly from EnergyPlus – produced by utility programs that read the .eso file and produce output files for spreadsheet programs</td>
</tr>
<tr>
<td>Eplusout.sln</td>
<td>Similar to DXF output but less “formatted”. Results of Report Surface, Lines object.</td>
</tr>
<tr>
<td>Epluszsz.&lt;ext&gt;</td>
<td>Results from the Zone Sizing object with extension noted by the Sizing Style object. This file is “spreadsheet” ready.</td>
</tr>
<tr>
<td>Eplusssz.&lt;ext&gt;</td>
<td>Results from the System Sizing object with extension noted by the Sizing Style object. This file is “spreadsheet” ready.</td>
</tr>
<tr>
<td>Eplusout.bnd</td>
<td>This file contains details about the nodes and branches. Useful in determining if all your nodes are connected correctly.</td>
</tr>
<tr>
<td>Eplustbl.&lt;ext&gt;</td>
<td>Results from various “Report Table” and a few other reporting commands. The extension is from the “style” command and this file is “spreadsheet” ready.</td>
</tr>
</table>

Errors
------

The **eplusout.err** file may contain three levels of errors (Information, Warning, Severe, Fatal) as well as the possibility of just message lines, see Table 48. EnergyPlus Errors and Required Actions. These errors may be duplicated in other files (such as the standard output file). These errors are described more completely in the Output Details and Examples document. An example .err file from the 1ZoneUncontrolled example input file:

```
Program Version,EnergyPlus 7.0.0.024, 10/9/2011 4:04 PM,IDD\_Version 7.0.0.024
   ** Warning ** IP: Note -- Some missing fields have been filled with defaults. See the audit output file for details.
   ** Warning ** Weather file location will be used rather than entered Location object.
   **   ~~~   ** ..Location object=DENVER STAPLETON INTL ARPT CO USA WMO=724690
   **   ~~~   ** ..Weather File Location=Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
   **   ~~~   ** ..due to location differences, Latitude difference=[3.00E-002] degrees, Longitude difference=[0.31] degrees.
   **   ~~~   ** ..Time Zone difference=[0.0] hour(s), Elevation difference=[13.53] percent, [218.00] meters.
   ************* Testing Individual Branch Integrity
   ************* All Branches passed integrity testing
   ************* Testing Individual Supply Air Path Integrity
   ************* All Supply Air Paths passed integrity testing
   ************* Testing Individual Return Air Path Integrity
   ************* All Return Air Paths passed integrity testing
   ************* No node connection errors were found.
   ************* Beginning Simulation
   ************* Simulation Error Summary *************
   ************* EnergyPlus Warmup Error Summary. During Warmup: 0 Warning; 0 Severe Errors.
   ************* EnergyPlus Sizing Error Summary. During Sizing: 0 Warning; 0 Severe Errors.
   ************* EnergyPlus Completed Successfully-- 2 Warning; 0 Severe Errors; Elapsed Time=00hr 00min  3.91sec
```

Table 48. EnergyPlus Errors and Required Actions

<table class="table table-striped">
<tr>
<th>Error Level</th>
<th>Action</th>
</tr>
<tr>
<td>Information, shown as *********</td>
<td>Informative, usually a follow-on to one of the others. No action required.</td>
</tr>
<tr>
<td>**   ~~~   **</td>
<td>This is a continuation of a previous message.  String all the words/sentences together to form the complete message.</td>
</tr>
<tr>
<td>Warning</td>
<td>Take note. Fix as applicable.</td>
</tr>
<tr>
<td>Severe</td>
<td>Should Fix</td>
</tr>
<tr>
<td>Fatal</td>
<td>Program will abort, Must Fix</td>
</tr>
</table>

EnergyPlus produces several messages as it is executing, as a guide to its progress. These message illustrate the sequence of execution as well as provides checkpoints should the simulation fail.

For example, the run of the 1ZoneUncontrolled example input file produces the log:
```
Running d:\eppgm\intcheck\energyplus.exe
ET Start: Sun 10/09/2011 - 16:04:34.71
Input File  : d:\devtests\inputfiles\checkin\1ZoneUncontrolled.idf
Output Files: d:\devtests\inputfiles\outputs\
Weather File: USA\_CO\_Golden-NREL.724666\_TMY3.epw
ExpandObjects Started.
No expanded file generated.
ExpandObjects Finished. Time:     0.016
 EnergyPlus Starting
 EnergyPlus 7.0.0.024, 10/9/2011 4:04 PM
 Processing Data Dictionary
 Processing Input File
 Initializing Simulation
 Reporting Surfaces
 Initializing New Environment Parameters
 Warming up {1}
 Warming up {2}
 Warming up {3}
 Warming up {4}
 Warming up {5}
 Warming up {6}
 Warming up {7}
 Warming up {8}
 Warming up {9}
 Warming up {10}
 Warming up {11}
 Warming up {12}
 Warming up {13}
 Warming up {14}
 Warming up {15}
 Warming up {16}
 Warming up {17}
 Warming up {18}
 Warming up {19}
 Warming up {20}
 Warming up {21}
 Warming up {22}
 Starting Simulation at 12/21 for DENVER STAPLETON INTL ARPT ANN HTG 99% CONDNS DB
 Initializing New Environment Parameters
 Warming up {1}
 Warming up {2}
 Warming up {3}
 Warming up {4}
 Warming up {5}
 Warming up {6}
 Warming up {7}
 Warming up {8}
 Warming up {9}
 Warming up {10}
 Warming up {11}
 Warming up {12}
 Warming up {13}
 Warming up {14}
 Warming up {15}
 Warming up {16}
 Warming up {17}
 Starting Simulation at 07/21 for DENVER STAPLETON INTL ARPT ANN CLG 1% CONDNS DB=&gt;MWB
 Initializing New Environment Parameters
 Warming up {1}
 Warming up {2}
 Warming up {3}
 Warming up {4}
 Warming up {5}
 Warming up {6}
 Warming up {7}
 Warming up {8}
 Warming up {9}
 Warming up {10}
 Warming up {11}
 Warming up {12}
 Warming up {13}
 Warming up {14}
 Warming up {15}
 Warming up {16}
 Warming up {17}
 Warming up {18}
 Warming up {19}
 Warming up {20}
 Starting Simulation at 01/01 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=01/21
 Continuing Simulation at 01/21 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=02/10
 Continuing Simulation at 02/10 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=03/02
 Continuing Simulation at 03/02 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=03/22
 Continuing Simulation at 03/22 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=04/11
 Continuing Simulation at 04/11 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=05/01
 Continuing Simulation at 05/01 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=05/21
 Continuing Simulation at 05/21 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=06/10
 Continuing Simulation at 06/10 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=06/30
 Continuing Simulation at 06/30 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=07/20
 Continuing Simulation at 07/20 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=08/09
 Continuing Simulation at 08/09 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=08/29
 Continuing Simulation at 08/29 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=09/18
 Continuing Simulation at 09/18 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=10/08
 Continuing Simulation at 10/08 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=10/28
 Continuing Simulation at 10/28 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=11/17
 Continuing Simulation at 11/17 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=12/07
 Continuing Simulation at 12/07 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Updating Shadowing Calculations, Start Date=12/27
 Continuing Simulation at 12/27 for Denver Centennial  Golden   Nr CO USA TMY3 WMO\#=724666
 Writing tabular output file results using comma format.
 Writing tabular output file results using tab format.
 Writing tabular output file results using text format.
 Writing tabular output file results using HTML format.
 EnergyPlus Run Time=00hr 00min  3.91sec
 ReadVarsESO program starting.
 ReadVars Run Time=00hr 00min  0.39sec
 ReadVarsESO program completed successfully.
 ReadVarsESO program starting.
 ReadVars Run Time=00hr 00min  0.27sec
 ReadVarsESO program completed successfully.
  Started HVAC Diagram
  Complete
note: Pausing=N
ET End: Sun 10/09/2011 - 16:04:39.34
```

In the example, the inclusion of the “ReadVars” program is also shown. It is run twice – once for the **eplusout.eso** file and once for the **eplusout.mtr** file.

EnergyPlus Execution Time
-------------------------

Extensive timing studies and fine-tuning of EnergyPlus are an ongoing process. Remember that hourly simulations cannot be directly compared to EnergyPlus with its less than hourly timesteps.

### Reducing Run Time Tips

Compared with creating energy models either by hand coding the IDF file or by using GUI tools or a combination of both, EnergyPlus run time is normally a small fraction of the total time needed to complete an energy modeling job. Therefore it is very important to build a clean and concise EnergyPlus model up front. Techniques of simplifying large and complex building and systems should be used during the creation of energy models, especially during the early design process when detailed zoning and other information is not available. Producing lots of hourly or sub-hourly reports from EnergyPlus runs can take significant amount of time. Modelers should only request time step reports when necessary. On the other hand, producing summary reports and typical monthly reports take relatively small amount of run time. These reports are valuable references for troubleshooting and model fine tuning.

With powerful personal computers get more and more affordable, EnergyPlus modelers should choose to use current available PCs with 3 or more GHZ and 3 or more GB of RAM. For a large volume of EnergyPlus parametric runs, modelers can launch multiple runs in parallel. For modelers, most time is spent on troubleshooting and fine tuning energy models. During the early modeling process, it is recommended to keep the model as simple as possible and make quick runs to identify problems. Then modify the IDF file to fix problems and re-run the model. This is an iterative process until satisfactory solutions are found. The simulation process can be split into three phases: the diagnostic runs, the preliminary runs, and the final runs. The three phases would use different simulation settings. The diagnostic runs would use a set of simulation settings to speed up the runs with simulation accuracy being set as the second priority. The diagnostic runs will help catch most model problems by

running simulations on summer and winter design days. The preliminary runs use a tighter set of simulation settings in order to catch problems missed in the diagnostic runs, and provide better results for quality assurance purpose. The final runs use the EnergyPlus recommended set of simulation settings in order to achieve better accuracy for simulation results ready for review and reporting.

You might want to read the report on EnergyPlus run time at http://repositories.cdlib.org/lbnl/LBNL-1311E/

Additional run-time tips may be found in the EnergyPlus "Tips and Tricks" document.  Some more tips:

- In the SimulationControl object, Run Simulation for Sizing Periods until everything is working well, then switch to Run Simulation for Weather File Run Periods.

- Turn off (comment them out in the text editor using !) daylighting controls while developing the HVAC simulation.  Then turn back on for final runs.

- HeatBalanceAlgorithm: ConductionTransferFunction is the fastest option.

- In the People object, the thermal comfort reports can be time consuming, especially the KSU model which is more computationally intensive.

