Input for Output
================

Several items are used to specify what will appear in the output file(s). The output is described in the next section of this document.

Group – Reports
---------------

### Variable Dictionary Reports

One of the most important outputs for EnergyPlus is the Variable Dictionary reports – these contain the key variable names for each simulation. This single input object is used to produce two important reports that are used to identify the specific output variables and meters available for a specific EnergyPlus model.  Two output files are produced (**eplusout.rdd & eplusout.mdd**) that list the names of the output variables and meters for that simulation.  You may need to run your simulation once and produce these files before you ask for specific output variables (**rdd**) or meters (**mdd**) (see Output:Variable and Output:Meter objects). Variables available, to some extent, depend on the simulation input. Variables are “set up” during the initial “get input” processing done within the modules. Therefore, an item that is specific to a certain type of coil would not be available if that coil were not used during the simulation. This command will produce a list of variables available for reporting.

#### Field: Key Field

For this field there are two choices: **IDF** and **regular**. The regular option is the default and produces a listing that shows the type of variable: Zone or HVAC, Average or Sum. “**Zone**” variables are calculated and can be reported after each Zone/Heat Balance timestep (ref: Timesteps input object). “**HVAC**” variables are calculated and can be reported with each variable HVAC timestep. “**Average**” variables will be averaged over the time interval being reported whereas “**Sum**” variables are summed over that time interval. (Meter variables are always summed.) Units for the variable are shown in “[}”.

#### Field: Sort Option

For this field there are two choices: **Name** and **Unsorted**. By default, the listing of available reporting variables are unsorted – listed in the streaming order that EnergyPlus generates them. Or, you can have them sorted by name.

Examples of the options for this object follow:

```idf
Output:VariableDictionary,regular;
```

Results of this are shown in the Output Details document under the **eplusout.rdd** (output variables) and **eplusout.mdd** (meter variables) files.

The eplusout.mdd file has a similar format for meters:

#### Variable Dictionary Report in IDF Format

The common workflow process of examining Variable Dictionary Reports and then adding reporting requests to an IDF file can be facilitated by using the following alternate form (Report Name = IDF) of the input object that requests the reports.

```idf
Output:VariableDictionary, IDF;
```

A sorted request would appear similarly:

```idf
Output:VariableDictionary, IDF, Name;
```

### Surface Reports

There are two specific objects for surfaces: **Output:Surfaces:List** and **Output:Surfaces:Drawing** – as the names imply, the list reports are simple text reports that are contained in one of the output files whereas the drawing reports will create files that are used in other graphic tools.

These objects and their use is described in the following:

### Output:Surfaces:List

Examples of using the Output:Surfaces:List object follow:

#### Lines Data Report

An example input object for this report follows.

```idf
Output:Surfaces:List, Lines;
```

The above IDF line will produce a simple file of line segments that constitute the surfaces in the IDF file. This file is the  “lines” report (**eplusout.sln**) and is decribed in more detail in the Output Details and Examples document.

#### Field: Report Specifications

An extra option of entering “IDF” in this field directs the Lines report to produce coordinates transformed into Surface Geometry (Lower Left Corner, CounterClockwise, WorldCoordinates).  An example of this follows.

```idf
Output:Surfaces:List, Lines, IDF;
```

The result is put on the **eplusout.sln** file and is nearly ready for putting into a IDF. Again, it is described in more detail in the Output Details and Examples document.

#### Detailed Surface Data Report

An example input object for this report follows.

```idf
Output:Surfaces:List, Details;
```

This report (in **eplusout.eio**) contains details about each surface – including surface name, associated zone, area, approximate height and width, tilt, outward facing angle. Specific details on this report can be found in the Output Details and Examples document.

#### Vertices Data Report

An example input object for this report follows.

```idf
Output:Surfaces:List, Vertices;
```

This report (in **eplusout.eio**) contains the vertices of each surface along with surface name and other information. Specific details on this report can be found in the Output Details and Examples document.

#### Detailed Surface Data With Vertices Report

An example input object for this report follows.

```idf
Output:Surfaces:List, DetailsWithVertices;
```

This report (in **eplusout.eio**) is the combination of the two preceding reports (Details and Vertices). Specific details on this report can be found in the Output Details and Examples document.

#### View Factor Information Report

An example input object for this report follows.

```idf
Output:Surfaces:List, ViewFactorInfo;
```

This report (in **eplusout.eio**) provides details about the thermal radiation exchange view factors and interchange factors.  Specific details on this report can be found in the Output Details and Examples document.

#### DecayCurvesfromZoneComponentLoads

An example input object for this report follows.

```idf
Output:Surfaces:List, DecayCurvesfromZoneComponentLoads;
```

This report (in **eplusout.eio**) provides an intermediate calculation used, called a decay curve,  in the Zone Component Loads Summary report for the estimate of the sensible-delayed column values. A decay curve is created for each surface in a zone by introducing a radiant pulse. Each value in each row of the report corresponds to a zone time step from the time the pulse was introduced. See Zone Component Load Summary described in object Output:Table:SummaryReports for more detailed information.

### Output:Surfaces:Drawing

Examples of using the Output:Surfaces:Drawing object follow:

#### DXF Surface Report

Examples input objects follow.

```idf
Output:Surfaces:Drawing, DXF;
Output:Surfaces:Drawing, DXF, RegularPolyline;
Output:Surfaces:Drawing, DXF, Triangulate3DFace, mycolorscheme;
```

The above IDF specification will produce a DXF file (**eplusout.dxf**) of the surfaces in the IDF file.  Only one DXF report can be produced. More specifics on this report can be found in the Output Details and Examples document.

#### Field: Report Specifications 1

As indicated in the example, you can select among three different ways that surfaces with more than four sides will appear in the DXF file. These are:

- ThickPolyline – surface with &gt;4 sides will be represented as a “thick” line in the appropriate surface color. It will look like a hole in the drawing with a thicker edge.

- RegularPolyline – surface with &gt;4 sides will be represented as a regular line in the appropriate surface color. It will look like a hole in the drawing.

- Triangulate3DFace– surface with &gt;4 sides will be “triangulated” internally within EnergyPlus.  This is only for drawing purposes and does not affect the calculations in any way.  In a line version of the DXF, it will appear that the surface is split into triangles. In a solid view, the surface will appear similar to surfaces with &lt;=4 sides. The triangulation algorithm is not perfect and warnings do result when the software cannot triangulate the surface.

#### Field: Report Specifications 2

This field can be used to control the color scheme in the DXF file by entering the name of an OutputControl:SurfaceColorScheme object.  Using OutputControl:SurfaceColorScheme, you can define color schemes for surface representation. This feature will let you align the colors with the software of your choice (colors don’t seem to be standard across DXF viewers).

Several software programs can render this file into something viewable and are described in more detail in the Output Details and Examples document under the file name (**eplusout.dxf**).

#### DXF Wire Frame Report

An example input object for this report follows.

```idf
Output:Surfaces:Drawing, DXF:WireFrame;
```

The above IDF specification will produce a DXF (Drawing Exchange Format) file (**eplusout.dxf**) of the surfaces in the IDF file using the DXF “Lines” command – producing only a wire frame capable file. More specifics on this report can be found in the Output Details and Examples document. Only one DXF report can be produced.

#### VRML Report

Examples input objects for this report follow.

```idf
Output:Surfaces:Drawing, VRML;
Output:Surfaces:Drawing, VRML, Triangulate3DFace;
```

The above IDF specification produces a VRML (Virtual Reality Modeling Language) file (**eplusout.wrl**) of the surfaces in the IDF file.  Only one VRML report can be produced. VRML files can be viewed in many web browsers after adding a “plug-in” and there are some stand-alone viewers as well. More specifics on this report can be found in the Output Details and Examples document.

#### Field: Report Specifications 1

As indicated in the example, you can select several figure types (for &gt;4 sided surfaces) in the DXF report. These are:

- ThickPolyline – surface with &gt;4 sides will be represented as a “thick” line in the appropriate surface color. It will look like a hole in the drawing with a thicker edge.

- RegularPolyline – surface with &gt;4 sides will be represented as a regular line in the appropriate surface color. It will look like a hole in the drawing.

- Triangulate3DFace – surface with &gt;4 sides will be “triangulated” internally within EnergyPlus.  This is only for drawing purposes and does not affect the calculations in any way.  In a line version of the VRML, it will appear that the surface is split into triangles. In a solid view, the surface will appear similar to surfaces with &lt;=4 sides. The triangulation algorithm is not perfect and warnings do result when the software cannot triangulate the surface.

#### Field: Report Specifications 2

Color schemes are not applicable for VRML reports.

### Output:Schedules

This is a condensed reporting that illustrates the full range of schedule values – in the style of input: DaySchedule, WeekSchedule, and Annual Schedule.

#### Field: Key Field

This field should contain the word **Hourly** or **TimeStep** to obtain the respective reports.

```idf
Output:Schedules, Hourly;  ! values on hourly increments (day schedule resolution)
Output:Schedules, TimeStep; ! will give them at the timestep of the simulation
```

This report is placed on the **eplusout.eio** file. Details of this reporting are shown in the Output Details and Examples document.  Schedule values over time can also be obtained in the EnergyPlus Standard Output (in eplusout.eso) using Output:Variable objects.

### Output:Constructions

#### Constructions Report

This report content is added to the “**eplusout.eio**” file. It shows the calculated results related to conduction transfer functions for each construction. It also includes similar details about the windows. Specific details on this report can be found in the Output Details and Examples document.  The following input object is used to obtain the Construction report.

```idf
Output:Constructions,Constructions;
```

#### Field: Details Type 1, Details Type 2

Either field can contain the word “**Constructions**” to get the constructions report.

Or either field can contain the word “**Materials**” to obtain the Materials report.

#### Materials Report

This report content is added to the “**eplusout.eio**” file. It provides a summary of the thermal properties and thickness of the construction and window materials in the model.  Specific details on this report can be found in the Output Details and Examples document.  The following input object is used to obtain the Materials report.

```idf
Output:Constructions,Materials;
```

### Output:DaylightFactors

Daylight factors are ratios of interior illuminance at a specific location to exterior horizontal illuminance. In EnergyPlus, they are pre-calculated hourly for every shadow calculation day for every exterior window of a daylight zone. The pre-calculated daylight factors are interpolated between hours and for the actual sky conditions in the time-step calculation of the daylighting performance of daylight zones.

EnergyPlus reports the daylight factors for four sky types (clear, turbid clear, intermediate, and overcast) at noon time (12:00pm) for each exterior windows (base window without shading) in the eio file when the daylight factors are first calculated. This new object can report all hourly pre-calculated daylight factors. Details of the report are shown in the Output Details document.

#### Field: Reporting Days

This field is used to select days to report the daylight factors. Two choices are “SizingDays” and “AllShadowCalculationDays”. The SizingDays choice will output the daylight factors for only the sizing calculation days, while the AllShadowCalculationDays choice will output daylight factors for all shadow calculation days.

### Output:EnergyManagementSystem

This report content is added to the “eplusout.edd” file. It shows information useful for the Energy Management System. The user can select the level of detail reported to the file using the fields in this object. However, there must be some other EMS-related input objects in the input file before this report will be generated.

#### Field: Actuator Availability Dictionary Reporting

This field is used to control the level of output reporting related to the EMS actuators that are available for a particular model. When EnergyPlus runs with EMS, it sets up a wide array of possible actuators that the EMS could use. (To actually use them requires an EnergyManagementSystem:Actuator object). Actuator availability dictionary reporting is provided to show the user what actuators are available in a particular building model. Regardless of the level of reporting chosen here, the same set of actuators are actually available. There are three levels to choose from. The “None” choice means that no reporting of available actuators is done. The “NotByUniqueNames” level means that the output includes only the types of actuators and their control options but not the unique, user-defined names that identify a specific actuator in the model. The “Verbose” level means that the output includes all combinations of actuator types, control types, and the unique names of specific actuators. The verbose level provides all the information needed for input in an EnergyManagementSystem:Actuator input object.

#### Field: Internal Variable Availability Dictionary Reporting

This field is used to control the level of output reporting related to the EMS internal variables that are available for a particular model. When EnergyPlus runs with EMS, it sets up a wide array of possible internal data sources that the EMS could use. (To actually use them requires an EnergyManagementSystem:InternalVariable object). Internal variable availability dictionary reporting is provided to show the user what internal data are available. Regardless of the level of reporting chosen here, the same internal data are available. There are three levels to choose from. The “None” choice means that no reporting of available internal data is done. The “NotByUniqueNames” level means that the output includes only the types of internal data but not the unique names that identify a specific instances in the model. The “Verbose” level means that the output includes all combinations of internal variables and the unique names of specific instances. The verbose level provides all the information needed for an EnergyManagementSystem:InternalVariable input object.

#### Field: EnergyPlus Runtime Language Debug Output Level

This field is used to control the level of output reporting related to the execution of EnergyPlus Runtime Language, or Erl. This reporting is valuable for debugging Erl programs. When Erl programs are run inside EnergyPlus they can report error situations (such as divide by zero) or a full trace of each Erl statement. There are three levels of reporting. The “None” choice means that no reporting of Erl debug information is done. The “ErrorsOnly” choice means that Erl debugging traces are only output if the statement produces an error situation. The “Verbose” choice means that Erl debugging traces are done for each line of each Erl program. The verbose setting needs to be used with care because a large model with a long runperiod can easily create an EDD file that is too large for most computer systems (e.g. many GBs of text).

An example of this object follows.

```idf
Output:EnergyManagementSystem,
    Verbose,    ! Actuator Availability Dictionary Reporting
    Verbose,    ! Internal Variable Availability Dictionary Reporting
    ErrorsOnly; ! EnergyPlus Runtime Language Debug Output Level
```

### OutputControl:SurfaceColorScheme

This object provides control over the colors that appear in the DXF report, by surface.  In addition, the Datasets folder contains example objects, including the “original” color scheme (prior to the 2.1 release).

#### Field: Name

This assigns a unique name to this colorscheme data set.  This name is used in the DXF output objects.

#### Field Set: Drawing Element Type and Colorr

This set of fields can range from none (accepting the default colors) up to 15 sets with individual types of building elements assigned a color number.  (drawing element types not assigned a color number will use the default color numbers)

#### Field: Drawing Element &lt;\#&gt; Type

This field uses a choice of the drawing types (Text, Walls, Windows, GlassDoors, Doors, Roofs, Floors, DetachedBuildingShades, DetachedFixedShades, AttachedBuildingShades, Photovoltaics, TubularDaylightDomes, TubularDaylightDiffusers, DaylightReferencePoint1, DaylightReferencePoint2) for the color number assignment in the following field.

#### Field: Color for Drawing Element &lt;\#&gt;

This is a color “number” from 0 to 255.  DXF display software is not standardized – you may need to play around with these numbers if you don’t like the supplied default color scheme.

### OutputControl:ReportingTolerances

This input object is created to allow more user control over some aspects of output reporting. Specifically, the reporting of “Time setpoint not met” hours  See thermostat reporting (ZoneControl Thermostat Outputs).

#### Field: Tolerance for Time Heating Setpoint Not Met

This field allows the entry of a value for the tolerance away from the heating setpoint reporting. If the zone temperature is below the heating setpoint by more than this value, the following output variables will increment as appropriate

Zone Heating Setpoint Not Met Time

Zone Heating Setpoint Not Met While Occupied Time

This also impacts table report "Annual Building Utility Performance Summary" subtable "Comfort and Setpoint Not Met Summary".

#### Field: Tolerance for Time Cooling Setpoint Not Met

This field allows the entry of a value for the tolerance away from the cooling setpoint reporting. If the zone temperature is above the cooling setpoint by more than this value, the following output variables will increment as appropriate

Zone Cooling Setpoint Not Met Time

Zone Cooling Setpoint Not Met While Occupied Time

This also impacts table report "Annual Building Utility Performance Summary" subtable "Comfort and Setpoint Not Met Summary".

As seen in an IDF:

```idf
OutputControl:ReportingTolerances,.21,.21;
```

### Output:Variable

This input object is used request results reporting.  As shown above in the Variable Dictionary Report section, there are many different output variables available for reporting results from EnergyPlus.  The Output:Variable object is primarily used for reporting time series data at various frequencys.

Each Output:Variable object causes a specific number assignment for outputs. For example, you could request separate reporting for the outside temperature:

```idf
Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;
Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;
Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;
```

#### Field: Key Value

This alpha field can be used to make a specific reference for reporting. In addition to the generic variable names listed in the Variable Dictionary Report for the input file, variables will also have a key designator (such as Zone name or Surface name). You can reference the standard output file (**eplusout.eso**) to see just how these look.

```
41,2,ZN001:WALL004,Surface Inside Temperature [C]
42,2,ZN001:WALL004,Surface Outside Temperature [C]
43,2,ZN001:WALL004,Surface Int Convection Coeff[W/m2-K]
44,2,ZN001:WALL004,Surface Ext Convection Coeff[W/m2-K]
46,2,ZONE ONE,Zone Mean Radiant Temperature [C]
47,2,ZONE ONE,Zone Total Internal Latent Gain Energy [J]
51,2,ZONE ONE,Zone Lights Electric Energy [J]
58,2,ZONE ONE,Zone Air Temperature [C]
```

For example, in the previous block, the key for the surface variables is **ZN001:WALL004** whereas the key for the zone variables is **ZONE ONE** (note that the space is required and significant for this key).

You can have all keys listed in the standard output file by putting a “\*” in this field or you can have specific items listed by putting in a key value. If this field is left blank, it will use a \* as the default (i.e., you will get all variables of Variable Name).

#### Field: Variable Name

This alpha field is the variable name (you don’t have to put on the units) that is shown in the Variable Dictionary Report file (eplusout.rdd). This field is required.

#### Field: Reporting Frequency

This field specifies how often the variable will be listed in the output file. “**Detailed**” will list the value each calculation step (i.e. Zone or HVAC). “**Timestep**” will be the same as “Detailed” for Zone valued variables and will be aggregated to the Zone timestep (i.e. Timestep in Hour value) for HVAC variables. “**Hourly**” will aggregate the value to the hour. “**Daily**” will aggregate to the day (i.e. one value per day). “**Monthly**” will aggregate to the month (i.e. one value per month). “**RunPeriod**” will aggregate to the runperiod specified (each Design Day is a runperiod as is each runperiod object). Default for this field if left blank or omitted is **Hourly**.

#### Field: Schedule Name

The final field is a schedule name. This can be used to limit the number of lines that appear in your output file. For example, a schedule such as “ON PEAK” or “OFF PEAK” could provide a slice of values. Or, a seasonal schedule could be devised. The output variable will be active during any hour in which the schedule value is &gt;0. For daily, monthly, and environment reporting frequencies, the aggregate value will be only for active schedule hours. Summed variables will report the sum for the active hours only. Averaged variables will report the average during the active hours only. If Schedule\_Name is omitted, the output variable will be active for all hours.

Other IDF examples:

```idf
Output:Variable, * , Zone Mean Air Temperature, hourly;
Output:Variable, * , Zone Mean Radiant Temperature, timestep;
Output:Variable, * , Zone Air System Sensible Heating Energy, hourly;
Output:Variable, * , Zone Air System Sensible Cooling Energy, hourly;
Output:Variable, * , Zone Air Temperature, hourly;
Output:Variable, * , Zone Air Temperature;  ! same as previous line
```


### Output:Meter and Output:Meter:MeterFileOnly

### Output:Meter:Cumulative and Output:Meter:Cumulative:MeterFileOnly

Appropriate variables are grouped onto “meters” for reporting purposes.  (The meters available are listed in the eplusout.mdd file, see Variable Dictionary Report section.) If the “Output:Meter” input object is used, these results written out to both the **eplusout.eso** and **eplusout.mtr** files. This allows easy graphing and comparison with “normal” values (such as Zone Temperature or Outdoor Temperature). If the “Output:Meter:MeterFileOnly” input object is used, then the values results written out to only the **eplusout.mtr** file. The **eplusout.mtr** file has the same structure as the **eplusout.eso** file. A companion file, the **eplusout.mtd** file, contains a detailed list and explanation of all meters for a given run and the component values that are accumulated on each meter.

The cumulative meter outputs (Output:Meter:Cumulative and  Output:Meter:Cumulative:MeterFileOnly) use the same meter names but the output is cumulative for the reporting (for each environment). That is, if the reporting frequency for the cumulative meter output is hourly, then hour 1 value will be that for hour 1, hour 2 value will be hour1 + hour2, etc. In other words, the end value for a cumulative meter reporting should be identical if you reported the value of the meter at the RunPeriod frequency.

#### Field: Name

Meter names applicable for the simulation are shown on the Variable Dictionary Report contained in the **eplusout.mdd** file described in the Output Details and Examples document.

Meter names are of three forms:

- &lt;ResourceType&gt;:&lt;name&gt;

Or

- &lt;EndUseType&gt;:&lt;ResourceType&gt;:&lt;name&gt;

Or

- &lt;EndUseSubcategory&gt;:&lt;EndUseType&gt;:&lt;ResourceType&gt;:&lt;name&gt;.

The user requests reporting by either specifying the full name above (without Units) or a “wildcard” representation of that name (to obtain all meters of a type). For example, entering “Electricity:\*” for the meter name will report on all the Electricity: meters (Electricity:Facility, Electricity:Building, Electricity:Zone:Resistive Zone, Electricity:Zone:East Zone, Electricity:North Zone in the example above). Both the resource types and end-use types are set within the program by the developers. End-use subcategories are user-defined and are an input field for certain objects.

To summarize the previous paragraph, you could use:

```idf
Output:Meter,Electricity:*;
```

To get the same outputs as if you had entered:

```idf
Output:Meter,Electricity:Facility;
Output:Meter,Electricity:Building;
Output:Meter,Electricity:Zone:South Zone;
Output:Meter,Electricity:Zone:East Zone;
Output:Meter,Electricity:North Zone;
```

From a three zone file that had no HVAC electricity (i.e., had only electric equipment or lighting equipment in an uncontrolled three zone building).

Current resource types are shown in the table below:

Table 38. Table of Metered Resource  Types

<table class="table table-striped">
<tr>
<th>Utility/Fuel Types</th>
</tr>
<tr>
<td>Electricity</td>
</tr>
<tr>
<td>Gas</td>
</tr>
<tr>
<td>Gasoline</td>
</tr>
<tr>
<td>Diesel</td>
</tr>
<tr>
<td>Coal</td>
</tr>
<tr>
<td>FuelOil#1</td>
</tr>
<tr>
<td>FuelOil#2</td>
</tr>
<tr>
<td>Propane</td>
</tr>
<tr>
<td>OtherFuel1</td>
</tr>
<tr>
<td>OtherFuel2</td>
</tr>
<tr>
<td>Water</td>
</tr>
<tr>
<td>Steam</td>
</tr>
<tr>
<td>DistrictCooling</td>
</tr>
<tr>
<td>DistrictHeating</td>
</tr>
<tr>
<td>ElectricityPurchased</td>
</tr>
<tr>
<td>ElectricitySurplusSold</td>
</tr>
<tr>
<td>ElectricityNet</td>
</tr>
</table>



<table class="table table-striped">
<tr>
<th>Other Resource Types</th>
</tr>
<tr>
<td>EnergyTransfer</td>
</tr>
</table>



The end use types are shown in the following table (note that certain end use types apply only to the EnergyTransfer resource):

Table 39. End Use Category Types

<table class="table table-striped">
<tr>
<th>Utility/Fuel End Use Types</th>
</tr>
<tr>
<td>InteriorLights</td>
</tr>
<tr>
<td>ExteriorLights</td>
</tr>
<tr>
<td>InteriorEquipment</td>
</tr>
<tr>
<td>ExteriorEquipment</td>
</tr>
<tr>
<td>Fans</td>
</tr>
<tr>
<td>Pumps</td>
</tr>
<tr>
<td>Heating</td>
</tr>
<tr>
<td>Cooling</td>
</tr>
<tr>
<td>HeatRejection</td>
</tr>
<tr>
<td>Humidifier</td>
</tr>
<tr>
<td>HeatRecovery</td>
</tr>
<tr>
<td>DHW</td>
</tr>
<tr>
<td>Cogeneration</td>
</tr>
<tr>
<td>Refrigeration</td>
</tr>
<tr>
<td>WaterSystems</td>
</tr>
</table>





<table class="table table-striped">
<tr>
<th>Additional End Use Types Only Used for EnergyTransfer</th>
</tr>
<tr>
<td>HeatingCoils</td>
</tr>
<tr>
<td>CoolingCoils</td>
</tr>
<tr>
<td>Chillers</td>
</tr>
<tr>
<td>Boilers</td>
</tr>
<tr>
<td>Baseboard</td>
</tr>
<tr>
<td>HeatRecoveryForCooling</td>
</tr>
<tr>
<td>HeatRecoveryForHeating</td>
</tr>
</table>

Specific meter types are then used for grouping the fuel type meters:

Table 40. Overall Meter Types

<table class="table table-striped">
<tr>
<th>Meters</th>
</tr>
<tr>
<td>Facility</td>
</tr>
<tr>
<td>Building</td>
</tr>
<tr>
<td>Zone</td>
</tr>
<tr>
<td>HVAC</td>
</tr>
<tr>
<td>Plant</td>
</tr>
</table>

Facility meters contain all the energy of a fuel type. Building meters contain the sum of each zone’s energy. HVAC meters contain the energy from the air and zone HVAC system components. Plant meters contain the energy from the plant equipment.

Thus, the following relationships should be observed:

<div>\[Facility = \sum {\left( {Building + HVAC + Plant + Exterior} \right)} \]</div>

<div>\[Building = \sum\limits_1^{NumberOfZones} {Zone} \]</div>

There are also some special purpose meters used to describe electricity resource flows in greater detail.  Electricity metering is tracked in more detail to accommodate the complexities of onsite electricity generation.  There are three variations on the electricity resource type including:  (1) “ElectrictyPurchased” which is the quantity of electricity purchased and is always positive, (2) “ElectricitySold” which is the quantity of electricity exported by the building, that which is typically “sold” back to the power company, and is always positive, and (3) “ElectricityNet” which is the net electricity demand considering all sources of onsite generation and demand and can be either positive or negative.

#### Field: Reporting Frequency

Similar to the output variables, meters can be reported at different frequencies. Their resolution is the Zone Timestep interval (Number of Timesteps input). For this frequency, the elemental value of the meter will be reported. For more inclusive frequencies, the summed value along with the maximum and minimum values for the time interval will be reported. Default for this field if left blank or omitted is **Hourly**.

Examples of specifying the object in the IDF follow.

```idf
Output:Meter,Electricity:*,RunPeriod;
Output:Meter,ExteriorEquipment*,RunPeriod;
Output:Meter:Cumulative,Electricity:*,Monthly;
```

#### Meter Details File (eplusout.mtd)

The meter details file illustrates what is on each meter and vice versa.  It is described in the Output Details and Examples document under the file name.

### Output:EnvironmentalImpactFactors

The Output:EnvironmentalImpactFactors object triggers the necessary Facility meters to be reported and the Pollutants resulting from these on and off-site energy consumptions. These values are put into the eplusout.mtr file. Facility meters contain all the energy of a fuel type. Thus, the following relationships should be observed:

<div>\[Facility = \sum {\left( {Building + System + Plant + Exterior} \right)} \]</div>

Information regarding how the coefficients are derived and applied can be found in the Engineering Document.

To turn on the reporting and the calculations for the environmental factors the user enters 3 objects:

**Output:EnvironmentalImpactFactors:** (with associated reporting frequency, more than one frequency can be requested)

**EnvironmentalImpactFactors:** (contains additional information necessary for the calculations not included in the Fuel Factors)

**FuelFactors:** (required for every fuel that is simulated and converted in this simulation input file)

Users will enter environmental impact factors for each Fuel Type or FuelFactor. This is the same for each Fuel Type: Natural Gas, Residual Oil, Distillate Oil, Coal, Off-Site Electricity, Gasoline, and Propane. For District Heating, District Cooling, and Steam there are additional fields in the EnvironmentalImpactFactors object that help in the conversion of their energy to Natural Gas and Electricity.

The energy is obtained internally from the following facility meters:

<table class="table table-striped">
<tr>
<th>Electricity:Facility</th>
<th>Diesel:Facility</th>
</tr>
<tr>
<td>DistrictCooling:Facility</td>
<td>DistrictHeating:Facility</td>
</tr>
<tr>
<td>Gas:Facility</td>
<td>Gasoline:Facility</td>
</tr>
<tr>
<td>Coal:Facility</td>
<td>FuelOil#1:Facility</td>
</tr>
<tr>
<td>FuelOil#2:Facility</td>
<td>Propane:Facility</td>
</tr>
<tr>
<td>ElectricityProduced:Facility</td>
<td>Steam:Facility</td>
</tr>
<tr>
<td>OtherFuel1:Facility</td>
<td>OtherFuel2:Facility</td>
</tr>
</table>

For the pollutant calculation only the total off-site or purchased electric energy is calculated using Electricity Factors. Off-Site Electricity = Electricity:Facility - ElectricityProduced:Facility, pollutants for the fuel type used by the on-site generator will be calculated, for example Diesel, in these cases.

#### Field: Reporting Frequency

Similar to the output variables and meters the environmental impact calculations can be reported at different frequencies. Their resolution is the Zone Timestep interval (Number of Timesteps input). For this frequency, the elemental value of the Environmental Impact Factors Calculation will be reported. For more inclusive frequencies, the summed value along with the maximum and minimum values for the time interval will be reported.

Valid values are Timestep, Hourly, Daily, Monthly or RunPeriod.

An example of specifying in the IDF:

```idf
  Output:EnvironmentalImpactFactors,
    Monthly;  !- Reporting_Frequency
```

### EnvironmentalImpactFactors

The EnvironmentalImpactFactors object contains additional information that is not individually specified in the FuelFactors object and is the second object needed to complete the calculation.  Source to Site conversion factors for District Heating, District Cooling and Steam are entered in this object.

#### Field: District Heating Efficiency

The District Heating Efficiency value is used to convert the district (purchased) heating energy to Natural Gas for the environmental impact calculation. The efficiency is a number between 0 and 1 and divided into the district heating energy and the default is 0.3.

#### Field: Disctrict Cooling COP

The District Cooling COP value is used to convert the district (purchased) cooling energy to electricity for the environmental impact calculation. The Coefficient of Performance (COP) is a number greater than 0 and divided into the purchased cooling energy with a default of 3.0.

#### Field: Steam Conversion Efficiency

The Steam Conversion Efficiency is used to convert the Steam usage to Natural Gas for the environmental impact calculation. The efficiency is a number between 0 and 1 and divided into the purchased heating energy and the default is 0.25.

#### Field: Total Carbon Equivalent Emission Factor From N2O

The Intergovernmental Panel on Climate Change has studied the effects on the relative radiative forcing effects of various greenhouse gases. This effect, called Global Warming Potential (GWP), is described in terms of the Carbon Equivalent of a particular greenhouse gase. N<sub>2</sub>O (nitrous oxide) can be produced by some Fuel Types and has a default of carbon equivalent emission factor of 80.7272 kg C/kg N<sub>2</sub>O.

#### Field: Total Carbon Equivalent Emission Factor From CH4

The Intergovernmental Panel on Climate Change has studied the effects on the relative radiative forcing effects of various greenhouse gases. This effect, called Global Warming Potential (GWP), is described in terms of the Carbon Equivalent of a particular greenhouse gas. CH<sub>4</sub> (methane) can be produced by all Fuel Types and has a default carbon equivalent emission factor of 6.2727 kg C/kg CH<sub>4</sub>.

#### Field: Total Carbon Equivalent Emission Factor From CO2

The Intergovernmental Panel on Climate Change has studied the effects on the relative radiative forcing effects of various greenhouse gases. This effect, called Global Warming Potential (GWP), is described in terms of the Carbon Equivalent of a particular greenhouse gas. CO<sub>2</sub> (carbon dioxide) can be produced by all Fuel Types and has a default carbon equivalent emission factor of 0.2727 kg C/kg CO<sub>2</sub>.

An example of specifying in the IDF:

```idf
  EnvironmentalImpactFactors,
    0.3,         !- Disctrict Heating Efficiency
    3.0,         !- District Cooling COP
    0.25,        !- Steam Conversion Efficiency
    80.7272,     !- Total Carbon Equivalent Emission Factor From N2O
    6.2727,      !- Total Carbon Equivalent Emission Factor From CH4
    0.2727;      !- Total Carbon Equivalent Emission Factor From CO2
```

### FuelFactors

#### Field: Existing Fuel Resource Name

This is the name of the Fuel that is being described with this object. Source to Site conversion factors for each tool are entered with these objects. Allowable Fuel Types are:  Electricity, Natural Gas, FuelOil\#1, FuelOil\#2, Coal, Gasoline, Propane, Diesel, OtherFuel1, OtherFuel2.

#### Field: Units of Measure

Units in mass with kg or by volume in m3.

#### Field: Energy per Unit Factor

The higher heating value of the fuel type.

#### Field: Source Energy Factor

Multiplied by the fuel consumption to compute the source energy contribution for the fuel. If a schedule is also specified in the next field, the value specified here, the schedule value, and the fuel consumption are multiplied together for each timestep to determine the source energy contribution for the fuel. If the multipliers in the schedule specified in the next field fully represent the source energy conversion factor, the value in this field should be set to one. If the TDV files (described in next field) are used the value should be 0.293 for electricity and 0.01 for natural gas to account for the units used in the files. Units of J/J.

#### Field: Source Energy Schedule Name

This field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the source energy. Specifically, each value in the schedule are multiplied by the Source Energy Factor field value and by the fuel consumption to determine the source energy consumption for the fuel. If the values in the schedule fully represent the source energy conversion factor, the value in the previous field, Source Energy Factor should be set to one.

The use of this field is required for evaluations using source energy factors that change throughout the year such as Time Dependent Valuation (TDV), the method used to show compliance with to California’s Title 24 under the ACM (Alternative Compliance Method). Data files containing TDV factors are in the DataSets\\TDV\\ directory. An example file for use with TDV file is in the ExampleFiles directory and is named 5zoneTDV.IDF. When using these TDV files with EnergyPlus it is important that a consistent weather file be used. It is also important that the day of the week for January 1 is set to Tuesday since all the TDV and related weather files use 1991 as the year of reference. Also note that the values in the TDV datasets files are in non SI units of measure. The source energy factor field should contain the value of 0.293 for electricity and 0.01 for natural gas to account for the non-standard units used in the files. A summary report that provides a breakdown of the facility energy use in terms of source energy can be obtained by selecting the '*AllSummary*' or '*SourceEnergyEndUseComponentsSummary*' keywords in the 'Output:Table:SummaryReports' reporting object. To learn more about TDV see:

http://www.energy.ca.gov/title24/2005standards/archive/rulemaking/documents/tdv/index.html

**WARNING**

**THE TDV CALCULATIONS SHOULD ONLY BE PERFORMED ON SIMULATIONS THAT USE THE CALIFORNIA THERMAL ZONE (CTZ REV2) WEATHER FILES. One must match the correct TDV file to its corresponding CTZ weather file. There are 16 TDV files, one for each of the 16 CTZ weather files. The Time Dependent Valuation of TDV files provides a relative cost estimate of the various fuel sources and their relative costs by time of day, month of year and outside temperature. Outside temperature is important as demand for electricity and thus cost of electricity is influenced by temperature. These TDV files are applicable only to the hourly weather data contained in the California Thermal Zone (CTZ rev2) weather files as the time dependent cost information is partially derived from the timing of temperatures in the weather file. Thus using the TDV files with other weather data would provide meaningless results.**

#### Field: CO2 Emission Factor

The environmental impact coefficient for the Fuel for calculating the mass of carbon dioxide (CO<sub>2</sub>) released into the atmosphere. The units are grams per MegaJoule. Carbon dioxide gas is naturally produced by animals during respiration and through decay of biomass, and used by plants during photosynthesis. Although it only constitutes 0.04 percent of the atmosphere, it is one of the most important greenhouse gases. The combustion of fossil fuels is increasing carbon dioxide concentrations in the atmosphere, which is believed to be contributing to global warming.

#### Field: CO2 Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: CO Emission Factor

The environmental impact coefficient for the Fuel for calculating the mass of carbon monoxide (CO) released into the atmosphere. The units are grams per MegaJoule. Carbon monoxide is a colorless, odorless and poisonous gas produced by incomplete fossil fuel combustion. Carbon monoxide combines with the haemoglobin of human beings, reducing its oxygen carrying capacity, with effects harmful to human beings.

#### Field: CO Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: CH4 Emission Factor

The environmental impact coefficient for the Fuel for calculating the mass of methane (CH<sub>4</sub>) released into the atmosphere. The units are grams per MegaJoule. Methane is a colorless, nonpoisonous, flammable gas created by anaerobic decomposition of organic compounds and is one of the more potent greenhouse gases. A major component of natural gas used in the home.

#### Field: CH4 Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: NOx Emission Factor

The environmental impact coefficient for the Fuel for calculating the mass of nitrogen oxides (NO<sub>x</sub>) released into the atmosphere. The units are grams per MegaJoule. Nitrogen oxides refers to nitric oxide gas (NO) and nitrogen dioxide gas (NO<sub>2</sub>) and many other gaseous oxides containing nitrogen. The main source of these gases in urban areas are motor vehicle exhaust and indoor gas stoves and kerosene heaters. The brown haze sometimes seen over cities is mainly nitrogen oxides. These gases are also partly responsible for the generation of ozone, which is produced when nitrogen oxides react with other chemicals in the presence of sunlight. Exposure to high levels of nitrogen dioxide can interfere with the ability of blood to carry oxygen, leading to dizziness and shortness of breath. Prolonged exposure can lead to respiratory failure.

#### Field: NOx Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: N2O Emission Factor

The environmental impact coefficient for the Fuel for calculating the mass of nitrous oxide (N<sub>2</sub>O) released into the atmosphere. The units are grams per MegaJoule. Relatively inert oxide of nitrogen produced as a result of microbial action in the soil, use of fertilizers containing nitrogen, burning of timber and coil, chemical industry, and so forth. This nitrogen compound may contribute to greenhouse and ozone-depleting effects.

#### Field: N2O Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: SO2 Emission Factor

The environmental impact coefficient for the Fuel for calculating the mass of sulfur dioxide (SO<sub>2</sub>) released into the atmosphere. The units are in grams per MegaJoule. Sulfur dioxide gas is formed when fuel containing sulfur, such as coal and oil, is burned, and when gasoline is extracted from oil, or metals are extracted from ore. Sulfur dioxide reacts with other chemicals in the air to form tiny sulfate particles, associated with increased respiratory symptoms and disease, difficulty in breathing, and premature death. Sulfur dioxide and nitrogen oxides react with other substances in the air to form acids, which fall to earth as rain, fog, snow, or dry particles. Acid rain damages forests and crops, changes the makeup of soil, and makes lakes and streams acidic and unsuitable for fish. Sulfur dioxide accelerates the decay of building materials and paints.

#### Field: SO2 Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: PM Emission Factor

The environmental impact coefficient for the Fuel for calculating the mass of particulate matter (PM) released into the atmosphere. The units are grams per MegaJoule. PM is the sum of all particular matter emitted, including PM10 and PM2.5. Particulate matter, or PM, are particles found in the air, including dust, dirt, soot, smoke, and liquid droplets, which can be suspended in the air for long periods of time. Some particles are large or dark enough to be seen as soot or smoke. Others are so small that individually they can only be detected with an electron microscope. Breathing particulate matter is linked to significant respiratory health problems.

#### Field: PM Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: PM10 Emission Factor

The environmental impact coefficient for the Fuel for calculating the mass of particulate matter 10 (PM<sub>10</sub>) released into the atmosphere. The units are grams per MegaJoule. PM<sub>10</sub>, includes particles with an aerodynamic diameter of less than 10 microns. These smaller particles are most likely responsible for the adverse health effects on humans because particles so small can reach the thoracic or lower regions of the respiratory tract.

#### Field: PM10 Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: PM2.5 Emission Factor

The environmental impact coefficient for the Fuel for calculating the mass of particulate matter 2.5 (PM<sub>2.5</sub>) released into the atmosphere. The units are grams per MegaJoule. EPA’s national air quality standards for fine particles, also known as “PM<sub>2.5</sub> standards,” are levels allowed in the outdoor air for particulate matter 2.5 microns in diameter or smaller. EPA issued the PM<sub>2.5</sub> standards in 1997 to protect human health and the environment. Studies have linked increased exposure to PM<sub>2.5</sub> to increases in premature death as well as a range of serious respiratory and cardiovascular effects.

#### Field: PM2.5 Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: NH3 Emission Factor

The Environmental impact coefficient for the Fuel for calculating the mass of ammonia (NH<sub>3</sub>) released into the atmosphere. The units are grams per MegaJoule. Ammonia reacts with nitrogen and sulfur compounds in the atmosphere, mainly nitric and sulfuric acids, to form particulate matter.

#### Field: NH3 Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: NMVOC Emission Factor

The Environmental impact coefficient for the Fuel for calculating the mass of non-methane volatile organic compounds (NMVOC) released into the atmosphere. The units are grams per MegaJoule. Non-methane volatile organic compounds (NMVOC), which include propane, butane, and ethane, are emitted primarily from transportation, industrial processes, and non-industrial consumption of organic solvents. Volatile organic compounds react with nitrogen oxides in the atmosphere to form ozone.

#### Field: NMVOC Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: Hg Emission Factor

The Environmental impact coefficient for the Fuel for calculating the mass lof mercury (Hg) released into the atmosphere. The units are grams per MegaJoule. This heavy metal can accumulate in the environment and is highly toxic if breathed or swallowed. In the U.S., primary sources of mercury air emissions are coal-fired power plants.

#### Field: Hg Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: Pb Emission Factor

The Environmental impact coefficient for the Fuel for calculating the masss of lead (Pb) released into the atmosphere. The units are grams per MegaJoule. A heavy metal that is hazardous to health if breathed or swallowed. Its use in gasoline, paints, and plumbing compounds has been sharply restricted or eliminated by federal laws and regulations.

#### Field: Pb Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: Water Emission Factor

The environmental impact coefficient for the Fuel for calculating the volume of water (H<sub>2</sub>O) consumed or evaporated in the generation of electricity. The units are liters per MegaJoule or a volume measurement. This is the water consumed in the production of the energy, ie. electricity off-site evaporated in cooling towers or scrubbers, or in the production or processing of the fuel itself, i.e., refinery for gasoline or diesel.

#### Field: Water Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: Nuclear High Level Emission Factor

The environmental impact coefficient for the Fuel for calculating the mass of high-level nuclear waste, removed as spent nuclear fuel from a nuclear reactor once it no longer is efficient at powering the reactor.. The units are grams per MegaJoule. Once a year, approximately one-third of nuclear fuel is replaced with new fuel. This used fuel is called spent nuclear fuel and is highly radioactive; containing plutonium and other radionuclides. Although there is little information on quantities of high-level nuclear waste, a few utilities are beginning to publish this information.

#### Field: Nuclear High Level Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

#### Field: Nuclear Low Level Emission factor

The environmental impact coefficient for the Fuel for calculating the volume of low-level nuclear waste, removed from a nuclear reactor after radiation contamination. The units are cubic meters per MegaJoule or a volume measurement. Low-level waste can come from nuclear reactors or other users of radioactive material, like hospitals or research institutes. Low-level waste is less hazardous than high-level waste.

#### Field: Nuclear Low Level Emission Factor Schedule Name

Similar to the source energy calculation, this field contains the name of a schedule containing values that are multiplied by the fuel consumption to determine the total emission values. Specifically, each value in the schedule are multiplied by the emission factor field value and by the fuel consumption to determine the overall emission factor for the fuel. If the values in the schedule fully represent the emission factor, the value in the previous field, should be set to one.

An example of specifying in the IDF:

```idf
FuelFactors,                 !  USA national average based on eGRID, EIA 1605
    Electricity,             !- Existing Fuel Resource Name
    kg,                      !- Units of Measure (kg or m3)
    ,                        !- Energy per Unit Factor
    2.253,                   !- Source Energy Factor {J/J}
    ,                        !- Source Energy Schedule Name
    168.33317,               !- CO2 Emission Factor {g/MJ}
    ,                        !- CO2 Emission Factor Schedule Name
    4.20616E-02,             !- CO Emission Factor {g/MJ}
    ,                        !- CO Emission Factor Schedule Name
    1.39858E-03,             !- CH4 Emission Factor {g/MJ}
    ,                        !- CH4 Emission Factor Schedule Name
    4.10753E-01,             !- NOx Emission Factor {g/MJ}
    ,                        !- NOx Emission Factor Schedule Name
    2.41916E-03,             !- N2O Emission Factor {g/MJ}
    ,                        !- N2O Emission Factor Schedule Name
    8.65731E-01,             !- SO2 Emission Factor {g/MJ}
    ,                        !- SO2 Emission Factor Schedule Name
    2.95827E-02,             !- PM Emission Factor {g/MJ}
    ,                        !- PM Emission Factor Schedule Name
    1.80450E-02,             !- PM10 Emission Factor {g/MJ}
    ,                        !- PM10 Emission Factor Schedule Name
    1.15377E-02,             !- PM2.5 Emission Factor {g/MJ}
    ,                        !- PM2.5 Emission Factor Schedule Name
    1.10837E-03,             !- NH3 Emission Factor {g/MJ}
    ,                        !- NH3 Emission Factor Schedule Name
    3.72332E-03,             !- NMVOC Emission Factor {g/MJ}
    ,                        !- NMVOC Emission Factor Schedule Name
    3.36414E-06,             !- Hg Emission Factor {g/MJ}
    ,                        !- Hg Emission Factor Schedule Name
    0,                       !- Pb Emission Factor {g/MJ}
    ,                        !- Pb Emission Factor Schedule Name
    2.10074,                 !- Water Emission Factor {L/MJ}
    ,                        !- Water Emission Factor Schedule Name
    0,                       !- Nuclear High Level Emission Factor {g/MJ}
    ,                        !- Nuclear High Level Emission Factor Schedule Name
    0;                       !- Nuclear Low Level Emission Factor {m3/MJ}
```


### Environmental Pollution Impact Outputs

Various output variables and meters related to emissions are available when modeling with environmental impact and fuel factor inputs.   Output is available as either output variables or output meters.   The variables and meters available depending on the number of FuelFactor input objects and the type of fuel used in them.  Most of the fuel types have the same set of outputs for emissions that correspond to the types of emission factors in the input objects.  The exception is electricity which has two additional outputs to track possible net metering situations that can occur with on-site generation.

The following pollution-emission-related outputs are available for environmental impacts.

* HVAC,Sum,Environmental Impact Natural Gas Source Energy [J]

* HVAC,Sum,Environmental Impact Natural Gas CO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas CO Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas CH4 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas NOx Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas N2O Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas SO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas PM Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas PM10 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas PM2.5 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas NH3 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas NMVOC Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas Hg Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas Pb Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Natural Gas Water Consumption Volume [L]

* HVAC,Sum,Nuclear High Level Waste From Natural Gas [kg]

* HVAC,Sum,Environmental Impact Natural Gas Nuclear Low Level Waste Volume [m3]

* HVAC,Sum,Environmental Impact Electricity Source Energy [J]

* HVAC,Sum,Environmental Impact Electricity CO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity CO Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity CH4 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity NOx Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity N2O Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity SO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity PM Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity PM10 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity PM2.5 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity NH3 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity NMVOC Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity Hg Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity Pb Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Electricity Water Consumption Volume [L]

* HVAC,Sum,Environmental Impact Electricity Nuclear High Level Waste Mass [kg]

* HVAC,Sum,Environmental Impact Electricity Nuclear Low Level Waste Volume [m3]

* HVAC,Sum,Environmental Impact Purchased Electricity Source Energy [J]

* HVAC,Sum,Environmental Impact Surplus Sold Electricity Source [J]

* HVAC,Sum,Environmental Impact Coal Source Energy [J]

* HVAC,Sum,Environmental Impact Coal CO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal CO Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal CH4 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal NOx Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal N2O Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal SO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal PM Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal PM10 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal PM2.5 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal NH3 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal NMVOC Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal Hg Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal Pb Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Coal Water Consumption Volume [L]

* HVAC,Sum,Environmental Impact Coal Nuclear High Level Waste Mass [kg]

* HVAC,Sum,Environmental Impact Coal Nuclear Low Level Waste Volume [m3]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 Source Energy [J]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 CO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 CO Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 CH4 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 NOx Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 N2O Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 SO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 PM Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 PM10 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 PM2.5 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 NH3 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 NMVOC Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 Hg Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 Pb Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 Water Consumption Volume [L]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 Nuclear High Level Waste Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#2 Nuclear Low Level Waste Volume [m3]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 Source Energy [J]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 CO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 CO Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 CH4 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 NOx Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 N2O Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 SO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 PM Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 PM10 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 PM2.5 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 NH3 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 NMVOC Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 Hg Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 Pb Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 Water Consumption Volume [L]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 Nuclear High Level Waste Mass [kg]

* HVAC,Sum,Environmental Impact Fuel Oil \#1 Nuclear Low Level Waste Volume [m3]

* HVAC,Sum,Environmental Impact Propane Source Energy [J]

* HVAC,Sum,Environmental Impact Propane CO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane CO Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane CH4 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane NOx Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane N2O Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane SO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane PM Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane PM10 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane PM2.5 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane NH3 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane NMVOC Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane Hg Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane Pb Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Propane Water Consumption Volume [L]

* HVAC,Sum,Environmental Impact Propane Nuclear High Level Waste Mass [kg]

* HVAC,Sum,Environmental Impact Propane Nuclear Low Level Waste Volume [m3]

* HVAC,Sum,Environmental Impact Gasoline Source Energy [J]

* HVAC,Sum,Environmental Impact Gasoline CO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline CO Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline CH4 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline NOx Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline N2O Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline SO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline PM Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline PM10 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline PM2.5 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline NH3 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline NMVOC Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline Hg Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline Pb Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline Water Consumption Volume [L]

* HVAC,Sum,Environmental Impact Gasoline Nuclear High Level Waste Mass [kg]

* HVAC,Sum,Environmental Impact Gasoline Nuclear Low Level Waste Volume [m3]

* HVAC,Sum,Environmental Impact Diesel Source Energy [J]

* HVAC,Sum,Environmental Impact Diesel CO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel CO Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel CH4 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel NOx Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel N2O Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel SO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel PM Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel PM10 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel PM2.5 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel NH3 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel NMVOC Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel Hg Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel Pb Emissions Mass [kg]

* HVAC,Sum,Environmental Impact Diesel Water Consumption Volume [L]

* HVAC,Sum,Environmental Impact Diesel Nuclear High Level Waste Mass [kg]

* HVAC,Sum,Environmental Impact Diesel Nuclear Low Level Waste Volume [m3]
* HVAC,Sum,Environmental Impact OtherFuel1 Source Energy [J]

* HVAC,Sum,Environmental Impact OtherFuel1 CO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 CO Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 CH4 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 NOx Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 N2O Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 SO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 PM Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 PM10 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 PM2.5 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 NH3 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 NMVOC Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 Hg Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 Pb Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 Water Consumption Volume [L]

* HVAC,Sum,Environmental Impact OtherFuel1 Nuclear High Level Waste Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel1 Nuclear Low Level Waste Volume [m3]

* HVAC,Sum,Environmental Impact OtherFuel2 Source Energy [J]

* HVAC,Sum,Environmental Impact OtherFuel2 CO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 CO Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 CH4 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 NOx Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 N2O Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 SO2 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 PM Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 PM10 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 PM2.5 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 NH3 Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 NMVOC Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 Hg Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 Pb Emissions Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 Water Consumption Volume [L]

* HVAC,Sum,Environmental Impact OtherFuel2 Nuclear High Level Waste Mass [kg]

* HVAC,Sum,Environmental Impact OtherFuel2 Nuclear Low Level Waste Volume [m3]


* HVAC,Sum,Environmental Impact Total N2O Emissions Carbon Equivalent Mass [kg]

* HVAC,Sum,Environmental Impact Total CH4 Emissions Carbon Equivalent Mass [kg]

* HVAC,Sum,Environmental Impact Total CO2 Emissions Carbon Equivalent Mass [kg]



#### Environmental Impact Electricity Source Energy [J]

#### Environmental Impact Purchased Electricity Source Energy [J]

#### Environmental Impact Surplus Sold Electricity Source [J]

#### Environmental Impact Natural Gas Source Energy [J]

#### Environmental Impact Fuel Oil \#2 Source Energy [J]

#### Environmental Impact Fuel Oil \#1 Source Energy [J]

#### Environmental Impact Coal Source Energy [J]

#### Environmental Impact Gasoline Source Energy [J]

#### Environmental Impact Propane Source Energy [J]

#### Environmental Impact Diesel Source Energy [J]

#### Environmental Impact OtherFuel1 Source Energy [J]

#### Environmental Impact OtherFuel2 Source Energy [J]

These outputs provide results for the source energy embodied in each type of fuel that might be used at the site.  The units are Joules. The EnergyPlus models energy use at the site and conversion factors are used to determine the added energy used to create, process or deliver a fuel to the building.  There are three types of electricity-related source energy results available.  The first “Environmental Impact Electricity Source Energy” is for the electricity consumed at the site that was not produced at the site.  The second, “Environmental Impact Purchased Electricity Source Energy,” is similar but includes just the electricity purchased form the utility.  The third, “Environmental Impact Surplus Sold Electricity Source,” is the source energy associated with any surplus electricity that is exported to the grid.

#### Environmental Impact Electricity CO2 Emissions Mass [kg]

#### Environmental Impact Natural Gas CO2 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 CO2 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 CO2 Emissions Mass [kg]

#### Environmental Impact Coal CO2 Emissions Mass [kg]

#### Environmental Impact Gasoline CO2 Emissions Mass [kg]

#### Environmental Impact Propane CO2 Emissions Mass [kg]

#### Environmental Impact Diesel CO2 Emissions Mass [kg]

#### Environmental Impact OtherFuel1 CO2 Emissions Mass [kg]

#### Environmental Impact OtherFuel2 CO2 Emissions Mass [kg]

These outputs provide results for the carbon dioxide reelases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity CO Emissions Mass [kg]

#### Environmental Impact Natural Gas CO Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 CO Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 CO Emissions Mass [kg]

#### Environmental Impact Coal CO Emissions Mass [kg]

#### Environmental Impact Gasoline CO Emissions Mass [kg]

#### Environmental Impact Propane CO Emissions Mass [kg]

#### Environmental Impact Diesel CO Emissions Mass [kg]

#### Environmental Impact OtherFuel1 CO Emissions Mass [kg]

#### Environmental Impact OtherFuel2 CO Emissions Mass [kg]

These outputs provide results for the carbon monoxide releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity CH4 Emissions Mass [kg]

#### Environmental Impact Natural Gas CH4 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 CH4 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 CH4 Emissions Mass [kg]

#### Environmental Impact Coal CH4 Emissions Mass [kg]

#### Environmental Impact Gasoline CH4 Emissions Mass [kg]

#### Environmental Impact Propane CH4 Emissions Mass [kg]

#### Environmental Impact Diesel CH4 Emissions Mass [kg]

#### Environmental Impact OtherFuel1 CH4 Emissions Mass [kg]

#### Environmental Impact OtherFuel2 CH4 Emissions Mass [kg]

These outputs provide results for the methane releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity NOx Emissions Mass [kg]

#### Environmental Impact Natural Gas NOx Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 NOx Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 NOx Emissions Mass [kg]

#### Environmental Impact Coal NOx Emissions Mass [kg]

#### Environmental Impact Gasoline NOx Emissions Mass [kg]

#### Environmental Impact Propane NOx Emissions Mass [kg]

#### Environmental Impact Diesel NOx Emissions Mass [kg]

#### Environmental Impact OtherFuel1 NOx Emissions Mass [kg]

#### Environmental Impact OtherFuel2 NOx Emissions Mass [kg]

These outputs provide results for the various nitrogen oxide releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity N2O Emissions Mass [kg]

#### Environmental Impact Natural Gas N2O Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 N2O Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 N2O Emissions Mass [kg]

#### Environmental Impact Coal N2O Emissions Mass [kg]

#### Environmental Impact Gasoline N2O Emissions Mass [kg]

#### Environmental Impact Propane N2O Emissions Mass [kg]

#### Environmental Impact Diesel N2O Emissions Mass [kg]

#### Environmental Impact OtherFuel1 N2O Emissions Mass [kg]

#### Environmental Impact OtherFuel2 N2O Emissions Mass [kg]

These outputs provide results for the nitrous oxide releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity SO2 Emissions Mass [kg]

#### Environmental Impact Natural Gas SO2 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 SO2 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 SO2 Emissions Mass [kg]

#### Environmental Impact Coal SO2 Emissions Mass [kg]

#### Environmental Impact Gasoline SO2 Emissions Mass [kg]

#### Environmental Impact Propane SO2 Emissions Mass [kg]

#### Environmental Impact Diesel SO2 Emissions Mass [kg]

#### Environmental Impact OtherFuel1 SO2 Emissions Mass [kg]

#### Environmental Impact OtherFuel2 SO2 Emissions Mass [kg]

These outputs provide results for the sulphur dioxide releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity PM Emissions Mass [kg]

#### Environmental Impact Natural Gas PM Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 PM Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 PM Emissions Mass [kg]

#### Environmental Impact Coal PM Emissions Mass [kg]

#### Environmental Impact Gasoline PM Emissions Mass [kg]

#### Environmental Impact Propane PM Emissions Mass [kg]

#### Environmental Impact Diesel PM Emissions Mass [kg]

#### Environmental Impact OtherFuel1 PM Emissions Mass [kg]

#### Environmental Impact OtherFuel2 PM Emissions Mass [kg]

These outputs provide results for particulate matter releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity PM10 Emissions Mass [kg]

#### Environmental Impact Natural Gas PM10 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 PM10 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 PM10 Emissions Mass [kg]

#### Environmental Impact Coal PM10 Emissions Mass [kg]

#### Environmental Impact Gasoline PM10 Emissions Mass [kg]

#### Environmental Impact Propane PM10 Emissions Mass [kg]

#### Environmental Impact Diesel PM10 Emissions Mass [kg]

#### Environmental Impact OtherFuel1 PM10 Emissions Mass [kg]

#### Environmental Impact OtherFuel2 PM10 Emissions Mass [kg]

These outputs provide results for particulate matter of 10 microns or less releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity PM2.5 Emissions Mass [kg]

#### Environmental Impact Natural Gas PM2.5 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 PM2.5 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 PM2.5 Emissions Mass [kg]

#### Environmental Impact Coal PM2.5 Emissions Mass [kg]

#### Environmental Impact Gasoline PM2.5 Emissions Mass [kg]

#### Environmental Impact Propane PM2.5 Emissions Mass [kg]

#### Environmental Impact Diesel PM2.5 Emissions Mass [kg]

#### Environmental Impact OtherFuel1 PM2.5 Emissions Mass [kg]

#### Environmental Impact OtherFuel2 PM2.5 Emissions Mass [kg]

These outputs provide results for particulate matter of 2.5 microns or less releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity NH3 Emissions Mass [kg]

#### Environmental Impact Natural Gas NH3 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 NH3 Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 NH3 Emissions Mass [kg]

#### Environmental Impact Coal NH3 Emissions Mass [kg]

#### Environmental Impact Gasoline NH3 Emissions Mass [kg]

#### Environmental Impact Propane NH3 Emissions Mass [kg]

#### Environmental Impact Diesel NH3 Emissions Mass [kg]

#### Environmental Impact OtherFuel1 NH3 Emissions Mass [kg]

#### Environmental Impact OtherFuel2 NH3 Emissions Mass [kg]

These outputs provide results for ammonia releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity NMVOC Emissions Mass [kg]

#### Environmental Impact Natural Gas NMVOC Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 NMVOC Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 NMVOC Emissions Mass [kg]

#### Environmental Impact Coal NMVOC Emissions Mass [kg]

#### Environmental Impact Gasoline NMVOC Emissions Mass [kg]

#### Environmental Impact Propane NMVOC Emissions Mass [kg]

#### Environmental Impact Diesel NMVOC Emissions Mass [kg]

#### Environmental Impact OtherFuel1 NMVOC Emissions Mass [kg]

#### Environmental Impact OtherFuel2 NMVOC Emissions Mass [kg]

These outputs provide results for non-methane volatile organic compound releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity Hg Emissions Mass [kg]

#### Environmental Impact Natural Gas Hg Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 Hg Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 Hg Emissions Mass [kg]

#### Environmental Impact Coal Hg Emissions Mass [kg]

#### Environmental Impact Gasoline Hg Emissions Mass [kg]

#### Environmental Impact Propane Hg Emissions Mass [kg]

#### Environmental Impact Diesel Hg Emissions Mass [kg]

#### Environmental Impact OtherFuel1 Hg Emissions Mass [kg]

#### Environmental Impact OtherFuel2 Hg Emissions Mass [kg]

These outputs provide results for mercury releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity Pb Emissions Mass [kg]

#### Environmental Impact Natural Gas Pb Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#2 Pb Emissions Mass [kg]

#### Environmental Impact Fuel Oil \#1 Pb Emissions Mass [kg]

#### Environmental Impact Coal Pb Emissions Mass [kg]

#### Environmental Impact Gasoline Pb Emissions Mass [kg]

#### Environmental Impact Propane Pb Emissions Mass [kg]

#### Environmental Impact Diesel Pb Emissions Mass [kg]

#### Environmental Impact OtherFuel1 Pb Emissions Mass [kg]

#### Environmental Impact OtherFuel2 Pb Emissions Mass [kg]

These outputs provide results for lead releases associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity Water Consumption Volume [L]

#### Environmental Impact Natural Gas Water Consumption Volume [L]

#### Environmental Impact Fuel Oil \#2 Water Consumption Volume [L]

#### Environmental Impact Fuel Oil \#1 Water Consumption Volume [L]

#### Environmental Impact Coal Water Consumption Volume [L]

#### Environmental Impact Gasoline Water Consumption Volume [L]

#### Environmental Impact Propane Water Consumption Volume [L]

#### Environmental Impact Diesel Water Consumption Volume [L]

#### Environmental Impact OtherFuel1 Water Consumption Volume [L]

#### Environmental Impact OtherFuel2 Water Consumption Volume [L]

These outputs provide results for water consumption or evaporation associated with consumption of each type of fuel that might be used at the site.  The units are liters.

#### Environmental Impact Electricity Nuclear High Level Waste Mass [kg]

#### Nuclear High Level Waste From Natural Gas [kg]

#### Environmental Impact Fuel Oil \#2 Nuclear High Level Waste Mass [kg]

#### Environmental Impact Fuel Oil \#1 Nuclear High Level Waste Mass [kg]

#### Environmental Impact Coal Nuclear High Level Waste Mass [kg]

#### Environmental Impact Gasoline Nuclear High Level Waste Mass [kg]

#### Environmental Impact Propane Nuclear High Level Waste Mass [kg]

#### Environmental Impact Diesel Nuclear High Level Waste Mass [kg]

#### Environmental Impact OtherFuel1 Nuclear High Level Waste Mass [kg]

#### Environmental Impact OtherFuel2 Nuclear High Level Waste Mass [kg]

These outputs provide results for high-level nuclear waste associated with consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Electricity Nuclear Low Level Waste Volume [m3]

#### Environmental Impact Natural Gas Nuclear Low Level Waste Volume [m3]

#### Environmental Impact Fuel Oil \#2 Nuclear Low Level Waste Volume [m3]

#### Environmental Impact Fuel Oil \#1 Nuclear Low Level Waste Volume [m3]

#### Environmental Impact Coal Nuclear Low Level Waste Volume [m3]

#### Environmental Impact Gasoline Nuclear Low Level Waste Volume [m3]

#### Environmental Impact Propane Nuclear Low Level Waste Volume [m3]

#### Environmental Impact Diesel Nuclear Low Level Waste Volume [m3]

#### Environmental Impact OtherFuel1 Nuclear Low Level Waste Volume [m3]

#### Environmental Impact OtherFuel2 Nuclear Low Level Waste Volume [m3]

These outputs provide results for low-level nuclear waste associated with consumption of each type of fuel that might be used at the site.  The units are cubic meters.

#### Environmental Impact Total N2O Emissions Carbon Equivalent Mass [kg]

This output provides the result for equivalent carbon mass associated with the nitrous oxide (N<sub>2</sub>O ) releases that stem from the consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Total CH4 Emissions Carbon Equivalent Mass [kg]

This output provides the result for equivalent carbon mass associated with the methane releases that stem from the consumption of each type of fuel that might be used at the site.  The units are kg.

#### Environmental Impact Total CO2 Emissions Carbon Equivalent Mass [kg]

This output provides the result for equivalent carbon mass associated with the carbon dioxide releases that stem from the consumption of each type of fuel that might be used at the site.  The units are kg.

The following different output meters could appear in the standard meter output (.mdd) file:

* Zone,Meter,Source:Facility [J]

* Zone,Meter,NaturalGasEmissions:Source [J]

* Zone,Meter,CO2:Facility [kg]

* Zone,Meter,NaturalGasEmissions:CO2 [kg]

* Zone,Meter,CO:Facility [kg]

* Zone,Meter,NaturalGasEmissions:CO [kg]

* Zone,Meter,CH4:Facility [kg]

* Zone,Meter,NaturalGasEmissions:CH4 [kg]

* Zone,Meter,NOx:Facility [kg]

* Zone,Meter,NaturalGasEmissions:NOx [kg]

* Zone,Meter,N2O:Facility [kg]

* Zone,Meter,NaturalGasEmissions:N2O [kg]

* Zone,Meter,SO2:Facility [kg]

* Zone,Meter,NaturalGasEmissions:SO2 [kg]

* Zone,Meter,PM:Facility [kg]

* Zone,Meter,NaturalGasEmissions:PM [kg]

* Zone,Meter,PM10:Facility [kg]

* Zone,Meter,NaturalGasEmissions:PM10 [kg]

* Zone,Meter,PM2.5:Facility [kg]

* Zone,Meter,NaturalGasEmissions:PM2.5 [kg]

* Zone,Meter,NH3:Facility [kg]

* Zone,Meter,NaturalGasEmissions:NH3 [kg]

* Zone,Meter,NMVOC:Facility [kg]

* Zone,Meter,NaturalGasEmissions:NMVOC [kg]

* Zone,Meter,Hg:Facility [kg]

* Zone,Meter,NaturalGasEmissions:Hg [kg]

* Zone,Meter,Pb:Facility [kg]

* Zone,Meter,NaturalGasEmissions:Pb [kg]

* Zone,Meter,WaterEnvironmentalFactors:Facility [L]

* Zone,Meter,NaturalGasEmissions:WaterEnvironmentalFactors [L]

* Zone,Meter,Nuclear High:Facility [kg]

* Zone,Meter,NaturalGasEmissions:Nuclear High [kg]

* Zone,Meter,Nuclear Low:Facility [m3]

* Zone,Meter,NaturalGasEmissions:Nuclear Low [m3]

* Zone,Meter,ElectricEmissions:Source [J]

* Zone,Meter,ElectricEmissions:CO2 [kg]

* Zone,Meter,ElectricEmissions:CO [kg]

* Zone,Meter,ElectricEmissions:CH4 [kg]

* Zone,Meter,ElectricEmissions:NOx [kg]

* Zone,Meter,ElectricEmissions:N2O [kg]

* Zone,Meter,ElectricEmissions:SO2 [kg]

* Zone,Meter,ElectricEmissions:PM [kg]

* Zone,Meter,ElectricEmissions:PM10 [kg]

* Zone,Meter,ElectricEmissions:PM2.5 [kg]

* Zone,Meter,ElectricEmissions:NH3 [kg]

* Zone,Meter,ElectricEmissions:NMVOC [kg]

* Zone,Meter,ElectricEmissions:Hg [kg]

* Zone,Meter,ElectricEmissions:Pb [kg]

* Zone,Meter,ElectricEmissions:WaterEnvironmentalFactors [L]

* Zone,Meter,ElectricEmissions:Nuclear High [kg]

* Zone,Meter,ElectricEmissions:Nuclear Low [m3]

* Zone,Meter,PurchasedElectricEmissions:Source [J]

* Zone,Meter,SoldElectricEmissions:Source [J]

* Zone,Meter,CoalEmissions:Source [J]

* Zone,Meter,CoalEmissions:CO2 [kg]

* Zone,Meter,CoalEmissions:CO [kg]

* Zone,Meter,CoalEmissions:CH4 [kg]

* Zone,Meter,CoalEmissions:NOx [kg]

* Zone,Meter,CoalEmissions:N2O [kg]

* Zone,Meter,CoalEmissions:SO2 [kg]

* Zone,Meter,CoalEmissions:PM [kg]

* Zone,Meter,CoalEmissions:PM10 [kg]

* Zone,Meter,CoalEmissions:PM2.5 [kg]

* Zone,Meter,CoalEmissions:NH3 [kg]

* Zone,Meter,CoalEmissions:NMVOC [kg]

* Zone,Meter,CoalEmissions:Hg [kg]

* Zone,Meter,CoalEmissions:Pb [kg]

* Zone,Meter,CoalEmissions:WaterEnvironmentalFactors [L]

* Zone,Meter,CoalEmissions:Nuclear High [kg]

* Zone,Meter,CoalEmissions:Nuclear Low [m3]

* Zone,Meter,FuelOil\#2Emissions:Source [J]

* Zone,Meter,FuelOil\#2Emissions:CO2 [kg]

* Zone,Meter,FuelOil\#2Emissions:CO [kg]

* Zone,Meter,FuelOil\#2Emissions:CH4 [kg]

* Zone,Meter,FuelOil\#2Emissions:NOx [kg]

* Zone,Meter,FuelOil\#2Emissions:N2O [kg]

* Zone,Meter,FuelOil\#2Emissions:SO2 [kg]

* Zone,Meter,FuelOil\#2Emissions:PM [kg]

* Zone,Meter,FuelOil\#2Emissions:PM10 [kg]

* Zone,Meter,FuelOil\#2Emissions:PM2.5 [kg]

* Zone,Meter,FuelOil\#2Emissions:NH3 [kg]

* Zone,Meter,FuelOil\#2Emissions:NMVOC [kg]

* Zone,Meter,FuelOil\#2Emissions:Hg [kg]

* Zone,Meter,FuelOil\#2Emissions:Pb [kg]

* Zone,Meter,FuelOil\#2Emissions:WaterEnvironmentalFactors [L]

* Zone,Meter,FuelOil\#2Emissions:Nuclear High [kg]

* Zone,Meter,FuelOil\#2Emissions:Nuclear Low [m3]

* Zone,Meter,FuelOil\#1Emissions:Source [J]

* Zone,Meter,FuelOil\#1Emissions:CO2 [kg]

* Zone,Meter,FuelOil\#1Emissions:CO [kg]

* Zone,Meter,FuelOil\#1Emissions:CH4 [kg]

* Zone,Meter,FuelOil\#1Emissions:NOx [kg]

* Zone,Meter,FuelOil\#1Emissions:N2O [kg]

* Zone,Meter,FuelOil\#1Emissions:SO2 [kg]

* Zone,Meter,FuelOil\#1Emissions:PM [kg]

* Zone,Meter,FuelOil\#1Emissions:PM10 [kg]

* Zone,Meter,FuelOil\#1Emissions:PM2.5 [kg]

* Zone,Meter,FuelOil\#1Emissions:NH3 [kg]

* Zone,Meter,FuelOil\#1Emissions:NMVOC [kg]

* Zone,Meter,FuelOil\#1Emissions:Hg [kg]

* Zone,Meter,FuelOil\#1Emissions:Pb [kg]

* Zone,Meter,FuelOil\#1Emissions:WaterEnvironmentalFactors [L]

* Zone,Meter,FuelOil\#1Emissions:Nuclear High [kg]

* Zone,Meter,FuelOil\#1Emissions:Nuclear Low [m3]

* Zone,Meter,PropaneEmissions:Source [J]

* Zone,Meter,PropaneEmissions:CO2 [kg]

* Zone,Meter,PropaneEmissions:CO [kg]

* Zone,Meter,PropaneEmissions:CH4 [kg]

* Zone,Meter,PropaneEmissions:NOx [kg]

* Zone,Meter,PropaneEmissions:N2O [kg]

* Zone,Meter,PropaneEmissions:SO2 [kg]

* Zone,Meter,PropaneEmissions:PM [kg]

* Zone,Meter,PropaneEmissions:PM10 [kg]

* Zone,Meter,PropaneEmissions:PM2.5 [kg]

* Zone,Meter,PropaneEmissions:NH3 [kg]

* Zone,Meter,PropaneEmissions:NMVOC [kg]

* Zone,Meter,PropaneEmissions:Hg [kg]

* Zone,Meter,PropaneEmissions:Pb [kg]

* Zone,Meter,PropaneEmissions:WaterEnvironmentalFactors [L]

* Zone,Meter,PropaneEmissions:Nuclear High [kg]

* Zone,Meter,PropaneEmissions:Nuclear Low [m3]

* Zone,Meter,GasolineEmissions:Source [J]

* Zone,Meter,GasolineEmissions:CO2 [kg]

* Zone,Meter,GasolineEmissions:CO [kg]

* Zone,Meter,GasolineEmissions:CH4 [kg]

* Zone,Meter,GasolineEmissions:NOx [kg]

* Zone,Meter,GasolineEmissions:N2O [kg]

* Zone,Meter,GasolineEmissions:SO2 [kg]

* Zone,Meter,GasolineEmissions:PM [kg]

* Zone,Meter,GasolineEmissions:PM10 [kg]

* Zone,Meter,GasolineEmissions:PM2.5 [kg]

* Zone,Meter,GasolineEmissions:NH3 [kg]

* Zone,Meter,GasolineEmissions:NMVOC [kg]

* Zone,Meter,GasolineEmissions:Hg [kg]

* Zone,Meter,GasolineEmissions:Pb [kg]

* Zone,Meter,GasolineEmissions:WaterEnvironmentalFactors [L]

* Zone,Meter,GasolineEmissions:Nuclear High [kg]

* Zone,Meter,GasolineEmissions:Nuclear Low [m3]

* Zone,Meter,DieselEmissions:Source [J]

* Zone,Meter,DieselEmissions:CO2 [kg]

* Zone,Meter,DieselEmissions:CO [kg]

* Zone,Meter,DieselEmissions:CH4 [kg]

* Zone,Meter,DieselEmissions:NOx [kg]

* Zone,Meter,DieselEmissions:N2O [kg]

* Zone,Meter,DieselEmissions:SO2 [kg]

* Zone,Meter,DieselEmissions:PM [kg]

* Zone,Meter,DieselEmissions:PM10 [kg]

* Zone,Meter,DieselEmissions:PM2.5 [kg]

* Zone,Meter,DieselEmissions:NH3 [kg]

* Zone,Meter,DieselEmissions:NMVOC [kg]

* Zone,Meter,DieselEmissions:Hg [kg]

* Zone,Meter,DieselEmissions:Pb [kg]

* Zone,Meter,DieselEmissions:WaterEnvironmentalFactors [L]

* Zone,Meter,DieselEmissions:Nuclear High [kg]

* Zone,Meter,DieselEmissions:Nuclear Low [m3]

* Zone,Meter,OtherFuel1Emissions:Source [J]

* Zone,Meter,OtherFuel1Emissions:CO2 [kg]

* Zone,Meter,OtherFuel1Emissions:CO [kg]

* Zone,Meter,OtherFuel1Emissions:CH4 [kg]

* Zone,Meter,OtherFuel1Emissions:NOx [kg]

* Zone,Meter,OtherFuel1Emissions:N2O [kg]

* Zone,Meter,OtherFuel1Emissions:SO2 [kg]

* Zone,Meter,OtherFuel1Emissions:PM [kg]

* Zone,Meter,OtherFuel1Emissions:PM10 [kg]

* Zone,Meter,OtherFuel1Emissions:PM2.5 [kg]

* Zone,Meter,OtherFuel1Emissions:NH3 [kg]

* Zone,Meter,OtherFuel1Emissions:NMVOC [kg]

* Zone,Meter,OtherFuel1Emissions:Hg [kg]

* Zone,Meter,OtherFuel1Emissions:Pb [kg]

* Zone,Meter,OtherFuel1Emissions:WaterEnvironmentalFactors [L]

* Zone,Meter,OtherFuel1Emissions:Nuclear High [kg]

* Zone,Meter,OtherFuel1Emissions:Nuclear Low [m3]

* Zone,Meter,OtherFuel2Emissions:Source [J]

* Zone,Meter,OtherFuel2Emissions:CO2 [kg]

* Zone,Meter,OtherFuel2Emissions:CO [kg]

* Zone,Meter,OtherFuel2Emissions:CH4 [kg]

* Zone,Meter,OtherFuel2Emissions:NOx [kg]

* Zone,Meter,OtherFuel2Emissions:N2O [kg]

* Zone,Meter,OtherFuel2Emissions:SO2 [kg]

* Zone,Meter,OtherFuel2Emissions:PM [kg]

* Zone,Meter,OtherFuel2Emissions:PM10 [kg]

* Zone,Meter,OtherFuel2Emissions:PM2.5 [kg]

* Zone,Meter,OtherFuel2Emissions:NH3 [kg]

* Zone,Meter,OtherFuel2Emissions:NMVOC [kg]

* Zone,Meter,OtherFuel2Emissions:Hg [kg]

* Zone,Meter,OtherFuel2Emissions:Pb [kg]

* Zone,Meter,OtherFuel2Emissions:WaterEnvironmentalFactors [L]

* Zone,Meter,OtherFuel2Emissions:Nuclear High [kg]

* Zone,Meter,OtherFuel2Emissions:Nuclear Low [m3]

* Zone,Meter,Carbon Equivalent:Facility [kg]

* Zone,Meter,CarbonEquivalentEmissions:Carbon Equivalent [kg]

What shows up in the output are the Energy Meters that were utilized in this particular input file, the pollutants, and the Total Carbon Equivalent.

### Output:SQLite

Output from EnergyPlus can be written to SQLite files for manipulation outside some of the standard (such as spreadsheet) tools.

#### Field: Option

Currently, there are limited capabilities for specifying SQLite generation.  The Simple option will include all of the predefined database tables as well as time series related data.  Using the SimpleAndTabular choice adds database tables related to the tabular reports that are already output by EnergyPlus in other formats.  The description for SQLite outputs is described fully in the Output Details document.

And one includes it in the IDF using the following.

```idf
Output:SQLite, Simple;
```

Output
======

EnergyPlus produces several output files as shown in the section on “Running EnergyPlus”.  This section will discuss the data contained in the “standard” output files (**eplusout.eso, eplusout.mtr**). They, too, have data dictionaries but unlike the input files, the output data dictionary is contained within the output file. Thus, the basic structure of the standard output file is:

```
Data Dictionary Information
End of Data Dictionary
Data
...
Data
End of Data
```


As with the IDF structure, there are rules associated with the interpretation of the standard output data dictionary. These rules are summarized as follows:

- The first item on each line is an integer which represents the “report code”. This “report code” will be listed in the data section where it will also be the first item on each line, identifying the data. Only 2 lines in the output file will not have an integer as the first item (“End of Data Dictionary” and “End of Data” lines).

- The second item on each line is also an integer. This integer corresponds to the number of items left on the dictionary line. Each string consists of a variable name and units in square brackets. Square brackets are required for all strings. If there are no units associated with a particular variable, then there are no characters between the brackets.

Six standard items appear at the start of every EnergyPlus Standard Output File Data Dictionary:

```
Program Version,EnergyPlus <version number indicated>
1,5,Environment Title[],Latitude[degrees],Longitude[degrees],Time Zone[],Elevation[m]
2,6,Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],Hour[],StartMinute[],EndMinute[],DayType
3,3,Cumulative Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],DayType  ! When Daily Output variables Requested
4,2,Cumulative Days of Simulation[],Month[]  ! When Monthly Output variables Requested
5,1,Cumulative Days of Simulation[] ! When Run Period Output variables Requested
```

* Item 0 is the program version statement.

* Item 1 is produced at the beginning of each new “environment” (design day, run period).

* Item 2 is produced prior to any variable reported at the timestep or hourly intervals. Hourly intervals will be shown with a start minute of 0.0 and an end minute of 60.0. Timestep intervals will show the appropriate start and end minutes.

* Item 3 is produced prior to any variable reported at the daily interval.

* Item 4 is produced prior to any variable reported at the monthly interval.

* Item 5 is produced prior to any variable reported at the end of the “environment”.

Following these five standard lines will be the variables requested for reporting from the input file (ref. Output:Variable object). For example:

```
6,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Hourly
40,1,ZONE ONE,Zone Total Internal Latent Gain Energy [J] !Hourly
68,1,ZONE ONE,Zone Mean Radiant Temperature [C] !Hourly
69,1,ZONE ONE,Zone Mean Air Temperature [C] !Hourly
70,1,ZONE ONE,Zone Air Heat Balance Surface Convection Rate [W] !Hourly
71,1,ZONE ONE,Zone Air Heat Balance Air Energy Storage Rate [W] !Hourly
```

This example illustrates the non-consecutive nature of the “report codes”. Internally, EnergyPlus counts each variable that *could* be reported. This is the assigned “report code”. However, the user may not request each possible variable for reporting. Note that, currently, the requested reporting frequency is shown as a comment (!) line in the standard output file.

The data is produced when the actual simulation is performed (after the warmup days unless the Output:Diagnostics requesting ReportDuringWarmup is used). Data output is simpler in format than the data dictionary lines. From the dictionary above:

```
1,DENVER STAPLETON INTL ARPT ANN HTG 99% CONDNS DB,  39.74,-105.18,  -7.00,1829.00
2,1,12,21, 0, 1, 0.00,60.00,WinterDesignDay
6,-16.
40,0.0
68,-19.8183039170649
69,-19.8220899203323
70,-3.175272922406513E-002
71,-3.181520440307718E-002
```


This output file can be easily turned into a form that is read into commonly used spreadsheet programs where it can be further analyzed, graphed, etc.

More details including graphs are shown in the Output Details and Examples Document under the file **eplusout.eso**.

Using ReadVarsESO
-----------------

### Creating Charts and Spreadsheet files from Output Variables

The ReadVarsESO program is distributed with EnergyPlus as a simple approach to converting the standard output files (**eplusout.eso, eplusout.mtr**) into files that can be put directly into a spreadsheet program and then used to create graphs or do other statistical operations. ReadVarsESO can read the complex output files and sort the data into time-based form, it is a very quick application but does not have a lot of features.  Note that all the **Output:Meter** and **Output:Meter:Cumulative** objects are included on the **eplusout.eso** file as well as the **eplusout.mtr** file.  You can choose the **Output:Meter:MeterFileOnly** or **Output:Meter:Cumulative:MeterFileOnly** objects if you do not want a particular meter to show up on the **eplusout.eso** file. If you wish to see only the metered outputs, the **eplusout.mtr** file will typically be a lot smaller than the **eplusout.eso** file.

The ReadVarsESO program has a very simple set of inputs. By default, you will get all the variables listed in the Output:Variable (**eplusout.eso**) or Output:Meter (**eplusout.mtr**) objects into the appropriate output files. The outputs from ReadVarsESO are limited to 255 variables (Microsoft Excel™ limit). You can tailor how many variables to list by specifying variables for the ReadVarsESO runs.

You can override the 255 variable limit by specifying an argument on the command line (**EP-Launch** has a special option for this). You use “unlimited” or “nolimit” on the command line to get as many variables into your output file as desired. Again, this will be limited either by the number of variables in the **eplusout.eso** or **eplusout.mtr** files or the contents of the “rvi” file. If you want to use this option, you must include two arguments to the command line of the ReadVars program – 1) the “rvi” file and 2) the “unlimited” argument.

Table 41. ReadVarsESO Command Line Options

<table class="table table-striped">
<tr>
<th>Option</th>
<th>Description</th>
</tr>
<tr>
<td>&lt;filename&gt;</td>
<td>To use any of these options, you must include a file name (“rvi” file) as the first argument.</td>
</tr>
<tr>
<td>Unlimited (or Nolimit)</td>
<td>Variables of any number will be produced on the output file.</td>
</tr>
<tr>
<td>Timestep</td>
<td>Only variables with reported frequency “timestep” (or detailed) will be produced on the output file.</td>
</tr>
<tr>
<td>Hourly</td>
<td>Only variables with reported frequency “hourly”  will be produced on the output file.</td>
</tr>
<tr>
<td>Daily</td>
<td>Only variables with reported frequency “daily”  will be produced on the output file.</td>
</tr>
<tr>
<td>Monthly</td>
<td>Only variables with reported frequency “monthly”  will be produced on the output file.</td>
</tr>
<tr>
<td>Annual (or RunPeriod)</td>
<td>Only variables with reported frequency “runperiod”  will be produced on the output file.</td>
</tr>
</table>

In addition, another argument can be used so that the output file is only one time frequency. (By default, all variables – hourly, monthly, annual, etc. are mixed together in the output file). By using “Timestep” as an argument, you would get only the TimeStep reported variables. Using “Monthly”, only the monthly variables. This is not automated in either **EP-Launch** or the **RunEPlus** batch file but can easily be accomplished.

The program is run automatically from the **EP-Launch** program or the **RunEPlus** batch file (both these methods for executing EnergyPlus are described in the GettingStarted document). These programs use **&lt;filename&gt;.rvi** for input to the ReadVarsESO program executed first after the EnergyPlus execution and **&lt;filename&gt;.mvi** for the second execution. Ostensibly, the .rvi file would apply to the eplusout.eso file and the .mvi would apply to the eplusout.mtr file, BUT the contents of the files actually specify the “**input**” reporting file and the “**output**” reorganized file. Typical contents of an .rvi file are:

Table 42. "RVI" file contents

<table class="table table-striped">
<tr>
<th>.rvi line description</th>
<th>Actual .rvi File Contents</th>
</tr>
<tr>
<td>Input File</td>
<td>eplusout.eso</td>
</tr>
<tr>
<td>Output File</td>
<td>eplusout.csv</td>
</tr>
<tr>
<td>Variable Name</td>
<td>Site Outdoor Drybulb Temperature</td>
</tr>
<tr>
<td>Variable Name</td>
<td>Zone Air Temperature</td>
</tr>
<tr>
<td>Variable Name</td>
<td>Zone Air Humidity Ratio</td>
</tr>
<tr>
<td>Variable Name</td>
<td>Zone Air System Sensible Cooling Rate</td>
</tr>
<tr>
<td>Variable Name</td>
<td>Zone Air System Sensible Heating Rate</td>
</tr>
<tr>
<td>Variable Name</td>
<td>Zone Total Internal Latent Gain Rate</td>
</tr>
<tr>
<td>Specific Variable Name</td>
<td>COOLING COIL AIR OUTLET NODE,System Node Temperature</td>
</tr>
<tr>
<td>Specific Variable Name</td>
<td>AIR LOOP OUTLET NODE,System Node Temperature</td>
</tr>
<tr>
<td>Specific Variable Name</td>
<td>AIR LOOP OUTLET NODE,System Node Humidity Ratio</td>
</tr>
<tr>
<td>Specific Variable Name</td>
<td>Mixed Air Node,System Node Mass Flow Rate</td>
</tr>
<tr>
<td>Specific Variable Name</td>
<td>Outdoor air Inlet Node,System Node Mass Flow Rate</td>
</tr>
<tr>
<td>Variable Name</td>
<td>Humidifier Water Consumption Rate</td>
</tr>
<tr>
<td>Variable Name</td>
<td>Humidifier Electric Power</td>
</tr>
<tr>
<td>Variable Name</td>
<td>Zone Air Relative Humidity</td>
</tr>
<tr>
<td>Variable Name</td>
<td>Zone Predicted Moisture Load Moisture Transfer Rate</td>
</tr>
<tr>
<td>Termination Line  (optional)</td>
<td>0</td>
</tr>
</table>

Note that the first two lines of the file are “input file” (where to read the output variable values from) and “output file” (where to put the reorganized data). If you have only those two lines in an “rvi” file, the program will use all the available variables on indicated input file to produce the output.

ReadVarsESO takes the input stream but recognizes the date/time information and appropriately places the required data onto the “output file”. Following these lines are a list of variables to be culled from the “input file” and placed onto the output file. “Variable Name” will take all variables matching that description whereas “Specific Variable Name” will only match the full description of the variable. So, in the above example, “Zone Air Temperature” will report air temperatures for all the zones (but available at the HVAC timestep if you choose the “detailed” reporting frequency in your input file) but “AIR LOOP OUTLET NODE” and “COOLING COIL AIR OUTLET NODE” will be the only values reported for the “System Node Temperature” variable (the node temperature is available for all nodes used in the simulation). The termination line (0) is included to terminate the input to the ReadVarsESO program and begin the scanning.

The output from ReadVarsESO is in the form commonly called “comma de-limited” or “comma separated variable”. This format can be read easily in spreadsheet programs, such as Excel™.

Note as described in the “Input for Output” above, only variables as listed on the **eplusout.rdd** file are available for reporting. If you request others, they will become “Warning” messages in the **eplusout.err** file.

```
** Warning ** The following Output variables were requested but not generated
**   ~~~   ** because IDF did not contain these elements or misspelled variable name -- check .rdd file
************* Key=*, VarName=SYSTEM SENSIBLE COOLING RATE
```

The above message was generated from an IDF that requested reporting of the “SYSTEM SENSIBLE COOLING RATE” but that variable was not available from the components in the execution.

Table 43. Example ReadVarsESO command lines and results

<table class="table table-striped">
<tr>
<th>Command Line</th>
<th>Description/Result</th>
</tr>
<tr>
<td>ReadVarsESO</td>
<td>Take eplusout.eso and produce an eplusout.csv file with all variables (up to 255) on it</td>
</tr>
<tr>
<td>ReadVarsESO my.rvi</td>
<td>Use the contents of “my.rvi” to produce the appropriate output file (limited to 255 variables)</td>
</tr>
<tr>
<td>ReadVarsESO my.rvi unlimited</td>
<td>Use the contents of “my.rvi” to produce the appropriate output file (no longer limited to 255 variables)</td>
</tr>
<tr>
<td>ReadVarsESO my.rvi Monthly</td>
<td>Use the contents of “my.rvi” to produce the appropriate output file and only produce those variables reported for “monthly” frequency (up to 255 variables)</td>
</tr>
<tr>
<td>ReadVarsESO my.rvi Daily unlimited</td>
<td>Use the contents of “my.rvi” to produce the appropriate output file and only produce those variables reported for “daily” frequency (no longer limited to 255 variables)</td>
</tr>
</table>

