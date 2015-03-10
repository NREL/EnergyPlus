# eplusout.sql

eplusout.sql is an optional output format for EnergyPlus. The eplusout.sql output file is a sqlite3 database file (see http://www.sqlite.org) and includes all of the data found in EnergyPlus' eplustbl.\* files, eplusout.eso and eplusout.mtr output files (i.e., EnergyPlus' standard variable and meter output files) plus a number of reports that are found in the eplusout.eio output file.

A discussion of the individual data tables is presented below followed by a discussion about how to access data within the SQL file.

## List of Available SQLite Tables

This initial release of the SQL database output option includes a variety of data in the following tables:

- ComponentSizes Table
- ConstructionLayers Table
- Constructions Table
- Materials Table
- NominalBaseboardHeaters Table
- NominalElectricEquipment Table
- NominalGasEquipment Table
- NominalHotWaterEquipment Table
- NominalInfiltration Table
- NominalLighting Table
- NominalOtherEquipment Table
- NominalPeople Table
- NominalSteamEquipment Table
- NominalVentilation Table
- ReportMeterData Table
- ReportMeterDataDictionary Table
- ReportMeterExtendedData Table
- ReportVariableData Table
- ReportVariableDataDictionary Table
- ReportVariableExtendedData Table
- RoomAirModels Table
- Schedules Table
- Surfaces Table
- SystemSizes Table
- Time Table
- ZoneGroups Table
- Zones Table
- ZoneLists Table
- ZoneSizes Table
- Simulations Table
- EnvironmentPeriods Table
- TabularData Table
- Strings Table
- StringTypes Table
- TabularDataWithStrings Table
- Errors Table

A short discussion of contents of each of the above SQL tables is given in the sections that follow.

## Report Variable Data

Data in the following four tables is also found in EnergyPlus' standard output file (i.e., eplusout.eso – see that section for more information). As with the standard output file, the "Report Variable" and "Report Meter" commands control the data in these tables.

### ReportVariableDataDictionary Table

The ReportVariableDataDictionary table provides the equivalent of the dictionary portion of the ESO file (i.e., the first section of the .eso file). Please see the Report Variable section of the Input-Output Reference for more information.

Table: SQL ReportVariableDataDictionary Contents

Field Name|Field Type|Description|Enumeration
----------|----------|-----------|-----------
ReportVariableDataDictionaryIndex|INTEGER PRIMARY KEY|The ReportVariableDataDictionaryIndex links the dictionary data to the variable data (see ReportVariableData table)|
VariableType|TEXT|Nature of data type with respect to state, (e.g. average or sum type of variable).|"Sum" "Avg"
IndexGroup|TEXT|The reporting group (e.g., Zone, Plant, etc.)|
TimestepType|TEXT|Type of timestep for data |"Zone"|"HVAC System"
KeyValue|TEXT|The identifying "key name" for the data|
VariableName|TEXT|The actual report variable name|
ReportingFrequency|TEXT|The reporting frequency of the variable, e.g. |"HVAC System Timestep",  "Zone Timestep",  "Hourly", "Daily", "Monthly", "Run Period".
ScheduleName|TEXT|The name of the schedule that controls reporting frequency|
VariableUnits|TEXT|The variable's units|

### ReportVariableData Table

The ReportVariableData table contains the report variable data (e.g., the hourly, daily, and monthly report variable data). Please see the Report Variable section of the Input-Output Reference for more information.

Table: SQL ReportVariableData Table Contents

Field Name|Field Type|Description
----------|----------|-----------
TimeIndex|INTEGER|This index links the record to its time record (see the Time table below)
ReportVariableDataDictionaryIndex|INTEGER|The ReportVariableDataDictionaryIndex links the data to the respective data dictionary record (see ReportVariableDataDictionary table above)
VariableValue|REAL|The variable's value
ReportVariableExtendedDataIndex|INTEGER |Links the record to its extended data, if any, such as minimums and maximums (see the ReportVariableExtendedData table below)

### ReportVariableExtendedData Table

The ReportVariableExtendedData table contains additional data (e.g., monthly maximums and minimums) that is available for certain report variables.

Table: SQL ReportVariableExtendedData Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ReportVariableExtendedDataIndex|INTEGER PRIMARY KEY|Connects the ReportVariableExtendedData table with the ReportVariableData table
MaxValue|REAL|The maximum value during the reporting interval
MaxMonth|INTEGER|The month in which the maximum value occurred
MaxDay|INTEGER|The day on which the maximum value occurred
MaxHour|INTEGER|The hour in which the maximum value occurred
MaxStartMinute|INTEGER|The starting minute of the interval in which the maximum value occurred
MaxMinute|INTEGER|The minute that the maximum value occurred
MinValue|REAL|The minimum value
MinMonth|INTEGER|The month in which the minimum value occurred
MinDay|INTEGER|The day on which the minimum value occurred
MinHour|INTEGER|The hour in which the minimum value occurred
MinStartMinute|INTEGER|The starting minute of the interval in which the minimum value occurred
MinMinute|INTEGER|The minute that the minimum value occurred

### Time Table

The Time table provides the time information for both the "report variable" and "report meter" variables (the ReportVariableData and ReportMeterData tables).

Table: Time Table Contents

Field Name|Field Type|Description
----------|----------|-----------
TimeIndex|INTEGER PRIMARY KEY|Connects the time information with the report variables and meter variables (see the ReportVariableData and ReportMeterData tables)
Month|INTEGER|Month
Day|INTEGER|Day
Hour|INTEGER|Hour
Minute|INTEGER|Minute
DST|INTEGER|Daylight saving time indicator
Interval|INTEGER|Length of the reporting interval
IntervalType|INTEGER|The index for the type of reporting interval. (enum: -1=detailed HVAC system timestep; 0=zone timestep, 1=hourly, 2=daily, 3=monthly, 4=runperiod)
SimulationDays|INTEGER|Day of simulation.  This number resets after warmup and at the beginning of an environment period
DayType|TEXT|The type of day. (enum: "Sunday", "Monday", "Tuesday", Wednesday", "Thursday", "Friday", "Saturday", "Holiday", "SummerDesignDay", "WinterDesignDay", "CustomDay1", "CustomDay2")
EnvironmentPeriodIndex|INTEGER|Foreign Key to the EnvironmentPeriods table
WarmupFlag|INTEGER|1 during warmup, 0 otherwise

## Report Meter Data

Data in the following three tables is also found in EnergyPlus' eplusout.mtr (i.e., meter) output file. Like in the eplusout.mtr file (see the eplusout.mtr section), only data requested by "Report Meter" commands is contained in the data tables below. Note that the Report Meter tables are very similar to the Report Variable tables described above.

### ReportMeterDataDictionary Table

The ReportMeterDataDictionary table provides the equivalent of the dictionary portion (i.e., the first section) of the eplusout.mtr file.

Table: SQL ReportMeterDataDictionary Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ReportMeterDataDictionaryIndex|INTEGER PRIMARY KEY|The ReportMeterDataDictionaryIndex links the dictionary data to the sampled data (see ReportMeterData table below)
VariableType|TEXT|Nature of data type with respect to state, (e.g. average or sum type of variable).
IndexGroup|TEXT|Groupings from meter classifications
TimestepType|TEXT|Type of timestep for data, (enum: "Zone" or "HVAC System")
KeyValue|TEXT|The identifying "key name" for the data record
VariableName|TEXT|The actual meter variable name
ReportingFrequency|TEXT|The reporting frequency of the variable. (enum: "HVAC System Timestep",  "Zone Timestep",  "Hourly", "Daily", "Monthly", "Run Period")
ScheduleName|TEXT|The name of the schedule that controls reporting frequency
VariableUnits|TEXT|The variable's units

### ReportMeterData Table

The ReportMeterData table contains the meter variable data (e.g., the hourly, daily, and monthly meter data).

Table: SQL ReportMeterData Table Contents

Field Name|Field Type|Description
----------|----------|-----------
TimeIndex|INTEGER|This index links the record to its time record (see the Time table below)
ReportMeterDataDictionaryIndex|INTEGER|The ReportMeterDataDictionaryIndex links the data to the respective data dictionary record (see the ReportMeterDataDictionary table)
VariableValue|REAL|The variable's value
ReportVariableExtendedDataIndex|INTEGER|Links the record to its extended data, if any  (see the ReportVariableExtendedData table below)

### ReportMeterExtendedData Table

The ReportMeterExtendedData table contains additional data (e.g., reporting interval maximums and minimums) that is available for certain meter variables.

Table: SQL ReportMeterExtendedData Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ReportMeterExtendedDataIndex|INTEGER PRIMARY KEY|The index that connects the extended data with the variable's primary data (see the ReportMeterData table)
MaxValue|REAL|The maximum value during the reporting period
MaxMonth|INTEGER|The month in which the maximum value occurred
MaxDay|INTEGER|The day on which the maximum value occurred
MaxHour|INTEGER|The hour in which the maximum value occurred
MaxStartMinute|INTEGER|The starting minute of the interval in which the maximum value occurred
MaxMinute|INTEGER|The minute in which the maximum value occurred
MinValue|REAL|The minimum value during the reporting period
MinMonth|INTEGER|The month in which the minimum value occurred
MinDay|INTEGER|The day on which the minimum value occurred
MinHour|INTEGER|The hour in which the minimum value occurred
MinStartMinute|INTEGER|The starting minute of the interval in which the minimum value occurred
MinMinute|INTEGER|The minute in which the minimum value occurred

## One time (EIO) File Data

Data in the tables below can also be found in EnergyPlus input output file (i.e., in the eplusout.eio output file).

### Zones Table

The Zones table provides a variety of information about the zones specified within EnergyPlus. One of its most common uses is to provide zone name and area information for the other tables within the SQL database (e.g., use the ZoneIndex to look up the ZoneName).

Table: SQL Zones Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ZoneIndex|INTEGER PRIMARY KEY|The ZoneIndex is used to link this table to related tables
ZoneName|TEXT|Zone Name
RelNorth|REAL|Relative North, in degrees
OriginX|REAL|X origin, in meters
OriginY|REAL|Y origin, in meters
OriginZ|REAL|Z origin, in meters
CentroidX|REAL|X position of zone Centroid, in meters
CentroidY|REAL|Y position of zone Centroid, in meters
CentroidZ|REAL|Z position of zone Centroid, in meters
OfType|INTEGER|(not used yet)
Multiplier|REAL|Zone multiplier
ListMultiplier|REAL|Zone Group multiplier
MinimumX|REAL|Minimum X value, in meters
MaximumX|REAL|Maximum X value, in meters
MinimumY|REAL|Minimum Y value, in meters
MaximumY|REAL|Maximum Y value, in meters
MinimumZ|REAL|Minimum Z value, in meters
MaximumZ|REAL|Maximum Z value, in meters
CeilingHeight|REAL|Ceiling height, in meters
Volume|REAL|Zone volume, in cubic meters
InsideConvectionAlgo|INTEGER|Inside convection algorithm (enum: 1=simple, 2=detailed, 3=Ceiling diffuser, 4=TrombeWall)
OutsideConvectionAlgo|INTEGER|Outside convection algorithm (enum: 1=simple, 2=detailed, 6=MoWitt, 7=DOE-2)
FloorArea|REAL|Zone floor area, in square meters
ExtGrossWallArea|REAL|Zone external gross wall area (includes windows and doors), in square meters
ExtNetWallArea|REAL|Zone net wall area (excludes windows and doors), and square meters
ExtWindowArea|REAL|Zone window area (includes glass doors), and square meters
IsPartOfTotalArea|INTEGER|See Zone input object documentation

Please see the Zone object in the Group-Thermal Zone Description/Geometry section of the Input-Output Reference for more information.

### NominalPeople Table

An overview of the NominalPeople SQL table is shown below.

Table: SQL NominalPeople Table Contents

Field Name|Field Type|Description
----------|----------|-----------
NominalPeopleIndex|INTEGER|The internal statement number
ObjectName|TEXT|The name of the People object
ZoneIndex|INTEGER|Connects the NominalPeople table to the Zones table
NumberOfPeople|INTEGER|Nominal number of people in the zone
NumberOfPeopleScheduleIndex|INTEGER|Number of people schedule number (see Schedule table)
ActivityScheduleIndex|INTEGER|Activity level schedule (see People object documentation)
FractionRadiant|REAL|see People object documentation
FractionConvected|REAL|see People object documentation
WorkEfficiencyScheduleIndex|INTEGER|Work efficiency schedule number (see schedule table and people object documentation)
ClothingEfficiencyScheduleIndex|INTEGER|Clothing efficiency schedule number (see schedule table and people object documentation)
AirVelocityScheduleIndex|INTEGER|Air velocity schedule number (see schedule table and people object documentation)
Fanger|INTEGER|Flag indicating whether Fanger calculations are active
Pierce|INTEGER|Flag indicating whether Pierce calculations are active
KSU|INTEGER|Flag indicating whether KSU calculations are active
MRTCalcType|INTEGER|see People object documentation
SurfaceIndex|INTEGER|see Surfaces table and People object documentation
UserSpecifeidSensibleFraction|REAL|see People object documentation
Show55Warning|INTEGER|see People object documentation

Please see the People object in the Group-Internal Gains section of the Input-Output Reference for more information.

### NominalLighting Table

An overview of the NominalLighting SQL table is shown below.

Table: SQL NominalLighting Table Contents

Field Name|Field Type|Description
----------|----------|-----------
NominalLightingIndex|INTEGER|The internal statement number
ObjectName|TEXT|The LIGHTS object name
ZoneIndex|INTEGER|Connects the NominalLighting table to the Zones table
ScheduleIndex|INTEGER|Lighting schedule number (see Schedule table)
DesignLevel|REAL|Nominal design level, in Watts
FractionReturnAir|REAL|User-specified return air fraction
FractionRadiant|REAL|User-specified radiant fraction
FractionReplaceable|REAL|Defines the daylighting control for the LIGHTS object
EndUseSubcategory|TEXT|User-specified end-use subcategory

Please see the LIGHTS object in the Group-Internal Gains section of the Input-Output Reference for more information.

### NominalElectricEquipment Table

An overview of the NominalElectricEquipment SQL table is shown below.

Table: SQL NominalElectricEquipment Table Contents

Field Name|Field Type|Description
----------|----------|-----------
NominalElectricEquipmentIndex|INTEGER|The internal statement number
ObjectName|TEXT|The Electric Equipment object name
ZoneIndex|INTEGER|Connects the NominalElectricEquipment table to the Zones table
ScheduleIndex|INTEGER|Electric equipment schedule number (see Schedule table)
DesignLevel|REAL|Nominal design level, in Watts
FractionLatent|REAL|User-specified latent heat fraction
FractionRadiant|REAL|User-specified radiant heat fraction
FractionLost|REAL|User-specified lost heat fraction
FractionConvected|REAL|User-specified convicted heat fraction
EndUseSubcategory|TEXT|User-specified end-use subcategory

Please see the Electric Equipment object in the Group-Internal Gains section of the Input-Output Reference for more information.

### NominalGasEquipment Table

An overview of the NominalGasEquipment SQL table is shown below.

Table: SQL NominalGasEquipment Table Contents

Field Name|Field Type|Description
----------|----------|-----------
NominalGasEquipmentIndex|INTEGER|The internal statement number
ObjectName|TEXT|The Gas Equipment object name
ZoneIndex|INTEGER|Connects the NominalGasEquipment table to the Zones table
ScheduleIndex|INTEGER|Gas equipment schedule number (see Schedule table)
DesignLevel|REAL|Nominal design level, in Watts
FractionLatent|REAL|User-specified latent heat fraction
FractionRadiant|REAL|User-specified radiant heat fraction
FractionLost|REAL|User-specified lost heat fraction
FractionConvected|REAL|User-specified convicted heat fraction
EndUseSubcategory|TEXT|User-specified end-use subcategory

Please see the Gas Equipment object in the Group-Internal Gains section of the Input-Output Reference for more information.

### NominalSteamEquipment Table

An overview of the NominalSteamEquipment SQL table is shown below.

Table: SQL NominalSteamEquipment Table Contents

Field Name|Field Type|Description
----------|----------|-----------
NominalSteamEquipmentIndex|INTEGER|The internal statement number
ObjectName|TEXT|The Steam Equipment object name
ZoneIndex|INTEGER|Connects the NominalSteamEquipment table to the Zones table
ScheduleIndex|INTEGER|Steam equipment schedule number (see Schedule table)
DesignLevel|REAL|Nominal design level, in Watts
FractionLatent|REAL|User-specified latent heat fraction
FractionRadiant|REAL|User-specified radiant heat fraction
FractionLost|REAL|User-specified lost heat fraction
FractionConvected|REAL|User-specified convicted heat fraction
EndUseSubcategory|TEXT|User-specified end-use subcategory

Please see the Steam Equipment object in the Group-Internal Gains section of the Input-Output Reference for more information.

### NominalHotWaterEquipment Table

An overview of the NominalHotWaterEquipment SQL table is shown below.

Table: SQL NominalHotWaterEquipment Table Contents

Field Name|Field Type|Description
----------|----------|-----------
NominalHotWaterEquipmentIndex|INTEGER|The internal statement number
ObjectName|TEXT|The Hot Water Equipment object name
ZoneIndex|INTEGER|Connects the NominalHotWaterEquipment table to the Zones table
ScheduleIndex|INTEGER|Hot water equipment schedule number (see Schedule table)
DesignLevel|REAL|Nominal design level, in Watts
FractionLatent|REAL|User-specified latent heat fraction
FractionRadiant|REAL|User-specified radiant heat fraction
FractionLost|REAL|User-specified lost heat fraction
FractionConvected|REAL|User-specified convicted heat fraction
EndUseSubcategory|TEXT|User-specified end-use subcategory

Please see the Hot Water Equipment object in the Group-Internal Gains section of the Input-Output Reference for more information.

### NominalOtherEquipment Table

An overview of the NominalOtherEquipment SQL table is shown below.

Table: SQL NominalOtherEquipment Table Contents

Field Name|Field Type|Description
----------|----------|-----------
NominalOtherEquipmentIndex|INTEGER|The internal statement number
ObjectName|TEXT|The Other Equipment object name
ZoneIndex|INTEGER|Connects the NominalOtherEquipment table to the Zones table
ScheduleIndex|INTEGER|Other equipment schedule number (see Schedule table)
DesignLevel|REAL|Nominal design level, in Watts
FractionLatent|REAL|User-specified latent heat fraction
FractionRadiant|REAL|User-specified radiant heat fraction
FractionLost|REAL|User-specified lost heat fraction
FractionConvected|REAL|User-specified convicted heat fraction
EndUseSubcategory|TEXT|User-specified end-use subcategory

Please see the Other Equipment object in the Group-Internal Gains section of the Input-Output Reference for more information.

### NominalBaseboardHeaters Table

An overview of the NominalBaseboardHeaters SQL table is shown below.

Table: SQL NominalBaseboardHeaters Table Contents

Field Name|Field Type|Description
----------|----------|-----------
NominalBaseboardHeaterIndex|INTEGER|The internal statement number
ObjectName|TEXT|The Baseboard Heat object name
ZoneIndex|INTEGER|Connects the NominalBaseboardHeat table to the Zones table
ScheduleIndex|INTEGER|Baseboard heat schedule number (see Schedules table)
CapatLowTemperature|REAL|Capacity at low temperature, in Watts
LowTemperature|REAL|Low temperature capacity setpoint
CapatHighTemperature|REAL|Capacity at high temperature, in Watts
HighTemperature|REAL|High temperature capacity setpoint
FractionRadiant|REAL|User-specified radiant heat fraction
FractionConvected|REAL|User-specified convicted heat fraction
EndUseSubcategory|TEXT|User-specified end-use subcategory

Please see the Baseboard Heat object in the Group-Internal Gains section of the Input-Output Reference for more information.

### NominalInfiltration Table

An overview of the NominalInfiltration SQL table is shown below.

Table: SQL NominalInfiltration Table Contents

Field Name|Field Type|Description
----------|----------|-----------
NominalInfiltrationIndex|INTEGER PRIMARY KEY|The internal statement number
ObjectName|TEXT|The Infiltration object name
ZoneIndex|INTEGER|Connects the NominalInfiltration table to the Zones table
ScheduleIndex|INTEGER|Infiltration schedule number (see Schedule table)
DesignLevel|REAL|Nominal design level, in m3/s

Please see the Infiltration object in the Group-Airflow section of the Input-Output Reference for more information.

### NominalVentilation Table

An overview of the NominalVentilation SQL table is shown below.

Table: SQL NominalVentilation Table Contents

Field Name|Field Type|Description
----------|----------|-----------
NominalVentilationIndex|INTEGER PRIMARY KEY|The internal statement number
ObjectName|TEXT|The Ventilation object name
ZoneIndex|INTEGER|Connects the NominalVentilation table to the Zones table
ScheduleIndex|INTEGER|Ventilation schedule number (see Schedule table)
DesignLevel|REAL|Nominal design level, in m3/s

Please see the Ventilation object in the Group-Airflow section of the Input-Output Reference for more information.

### Surfaces Table

An overview of the Surfaces SQL table is shown below.

Table: SQL Surfaces Table Contents

Field Name|Field Type|Description
----------|----------|-----------
SurfaceIndex|INTEGER PRIMARY KEY|Surface number (used for cross-referencing)
SurfaceName|TEXT |Surface name
ConstructionIndex|INTEGER|Construction Index
ClassName|TEXT|Surface class name (e.g., shading, wall)
Area|REAL|Surface area (excluding cutouts)
GrossArea|REAL|Surface area (including cutouts)
Perimeter|REAL|Surface perimeter, in meters
Azimuth|REAL|As news angle, in degrees
Height|REAL|Surface height, in meters
Reveal|REAL|Reveal depth, in meters
Shape|INTEGER|Shape index
Sides|INTEGER|Number of sides
Tilt|REAL|Tilt angle, in degrees
Width|REAL|Surface width, in meters
HeatTransferSurf|INTEGER|Flag indicating whether the surface is a heat transfer surface
BaseSurfaceIndex|INTEGER|Based surface index
ZoneIndex|INTEGER|Zone index
ExtBoundCond|INTEGER|External boundary condition index. For interzone surface, this is the adjacent surface number. For an internal/adiabatic surface this is the current surface number. 0=external environment, -1=ground, -2=other side coefficients, -3=other side conditions model.
ExtSolar|INTEGER|Flag indicating whether the surface is exposed to solar
ExtWind|INTEGER|Flag indicating whether the surface is exposed to wind

Please see the Surface(s) object in the Group-Thermal Zone Description/Geometry section of the Input-Output Reference for more information.

### Constructions Table

An overview of the Constructions SQL table is shown below.

Table: SQL Constructions Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ConstructionIndex|INTEGER PRIMARY KEY|Construction Index
Name|TEXT|Construction name
TotalLayers|INTEGER|Total number of layers
TotalSolidLayers|INTEGER|Total number of solid layers
TotalGlassLayers|INTEGER|Total number of glass layers
InsideAbsorpVis|REAL|The visible absorptance of the inside layer (see Materials table)
OutsideAbsorpVis|REAL|The visible absorptance of the outside layer (see Materials table)
InsideAbsorpSolar|REAL|The solar absorptance of the inside layer (see Materials table)
OutsideAbsorpSolar|REAL|The solar absorptance of the outside layer (see Materials table)
InsideAbsorpThermal|REAL|The thermal absorptance of the inside layer (see Materials table)
OutsideAbsorpThermal|REAL|The thermal absorptance of the outside layer (see Materials table)
OutsideRoughness|INTEGER|The roughness of the outside layer
TypeIsWindow|INTEGER|Flag indicating whether the construction is a window or glass door
Uvalue|REAL|Nominal U-value for the construction

Please see the Construction object in the Group-Surface Construction Elements section of the Input-Output Reference for more information.

### ConstructionLayers Table

An overview of the ConstructionLayers SQL table is shown below.

Table: SQL ConstructionLayers Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ConstructionIndex|INTEGER|Construction Index (see Constructions table)
LayerIndex|INTEGER|Layer number (layer 1 is the outside layer)
MaterialIndex|INTEGER|Material index (see Materials table)

Please see the Construction object in the Group-Surface Construction Elements section of the Input-Output Reference for more information.

### Materials Table

An overview of the Materials SQL table is shown below.

Table: SQL Materials Table Contents

Field Name|Field Type|Description
----------|----------|-----------
MaterialIndex|INTEGER PRIMARY KEY|Material Index (links the Materials table with the ConstructionLayers and Constructions tables)
Name|TEXT|Material name
MaterialType|INTEGER|Material type
Roughness|INTEGER|Roughness index
Conductivity|REAL|Conductivity, in W/(m-K)
Density|REAL|Density, in kg/m3
Porosity|REAL|Porosity
Resistance|REAL|Resistance
Ronly|INTEGER|Flag that indicates the material definition is of type Material:Regular-R
SpecHeat|REAL|Specific heat
ThermGradCoef|REAL|Thermal gradient coefficient
Thickness|REAL|Thickness, in meters
VaporDiffus|REAL|Vapor diffusivity

Please see the Materials object in the Group-Surface Construction Elements section of the Input-Output Reference for more information.

### RoomAirModels Table

An overview of the RoomAirModels SQL table is shown below. Please see the Group-RoomAir Models section of the Input-Output Reference for more information.

Table: SQL RoomAirModels Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ZoneIndex|INTEGER PRIMARY KEY|Zone index
AirModelName|TEXT|Air model name
AirModelType|INTEGER|Air model index
TempCoupleScheme|INTEGER|Temperature coupling index
SimAirModel|INTEGER|Simulation air model index

### ComponentSizes Table

An overview of the ComponentSizes SQL table is shown below.

Table: SQL ComponentSizes Table Contents

Field Name|Field Type|Description
----------|----------|-----------
CompType|TEXT|Component type
CompName|TEXT|Component name
Description|TEXT|Component description
Value|REAL|Sizing value
Units|TEXT|Sizing units

Please see the Sizing object in the Group-Design Objects section of the Input-Output Reference for more information.

### SystemSizes Table

An overview of the SystemSizes SQL table is shown below. Please see the System Sizing object in the Group-Design Objects section of the Input-Output Reference for more information.

Table: SQL SystemSizes Table Contents

Field Name|Field Type|Description
----------|----------|-----------
SystemName|TEXT|System name
Description|TEXT|System description
Value|REAL|Sizing value
Units|TEXT|Sizing units

### ZoneSizes Table

An overview of the ZoneSizes SQL table is shown below.

Table: SQL ZoneSizes Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ZoneName|TEXT|Zone name
LoadType|TEXT|Load type
DesLoad|REAL|Design load
CalcDesFlow|REAL|Calculated design flow
UserDesFlow|REAL|User-specified design flow
DesDayName|TEXT|Design day name
PeakHrMin|TEXT|Time of the peak temperature
PeakTemp|REAL|Peak temperature
PeakHumRat|REAL|Peak humidity ratio
CalcOutsideAirFlow|REAL|Calculated outside air flow rate, in m3/s

Please see the Zone Sizing object in the Group-Design Objects section of the Input-Output Reference for more information.

### ZoneGroups Table

An overview of the ZoneGroups SQL table is shown below.

Table: SQL ZoneGroups Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ZoneGroupIndex|INTEGER PRIMARY KEY|Zone group index
Name|TEXT|Zone list name
ZoneListMultiplier|INTEGER|Zone list multiplier

Please see the Zone Group object in the Group-Thermal Zone Description/Geometry section of the Input-Output Reference for more information.

### ZoneLists Table

An overview of the ZoneLists SQL table is shown below.

Table: SQL ZoneLists Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ZoneListIndex|INTEGER PRIMARY KEY|Zone list index
Name|TEXT|Zone list name
ZoneIndex|INTEGER|Zone index

Please see the Zone List object in the Group-Thermal Zone Description/Geometry section of the Input-Output Reference for more information.

## Miscellaneous Tables

### Schedules Table

An overview of the Schedules SQL table is shown below.

Table: SQL Schedules Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ScheduleIndex|INTEGER PRIMARY KEY|Schedule index
ScheduleName|TEXT|Schedule name
ScheduleType|TEXT|Schedule Type
ScheduleMinimum|REAL|ScheduleMinimum
ScheduleMaximum|REAL|ScheduleMaximum

Please see the Group-Schedules section of the Input-Output Reference for more information.

### Simulations Table

An overview of the Simulations SQL table is shown below.  Currently there will only be one record in the table, because the SQLite database is relevant to only one simulation.  In the future this might change if multiple simulations are aggregated into a larger database.

Table: SQL Simulations Table Contents

Field Name|Field Type|Description
----------|----------|-----------
SimulationIndex|INTEGER PRIMARY KEY|Simulation index, currently there will be only one record in this table.
EnergyPlusVersion|TEXT|The version of EnergyPlus that was used to run the simulation.
TimeStamp|TEXT|A text string containing the timestamp when the simulation was run.
NumTimestepsPerHour|INTEGER|The number of timesteps per hour used for the simulation.  See the Timestep object for more information.
Completed|BOOL|True if the simulation completed without crashing.
CompletedSuccessfully|BOOL|True if the simulation completed without any severe or fatal errors.

### EnvironmentPeriods Table

An overview of the EnvironmentPeriods SQL table is shown below.

Table: SQL EnvironmentPeriods Table Contents

Field Name|Field Type|Description
----------|----------|-----------
EnvironmentPeriodIndex|INTEGER PRIMARY KEY|Environment period index
SimulationIndex|INTEGER|A foreign key to a record in the Simulations table.
EnvironmentName|TEXT|A text string identifying the environment.
EnvironmentType|INTEGER|An enumeration of the environment type.  (1 = Design Day, 2 =  Design Run Period, 3 = Weather Run Period)  See the various SizingPeriod objects and the RunPeriod object for details.

### TabularData Table

An overview of the TabularData SQL table is shown below.   The TabularData table is only output when the "SimpleAndTabular" choice is made in the Output:SQLite object.  The TabularData table contains all of the tabular report information that is typically output to one of the formats controlled by the OutputControl:Table:Style object.  This is a generic way of dumping all of the existing tabular reports to the SQLite database.  This table has many references to the companion Strings table.  The Strings table simply associates strings to an integer value for space efficiency of the database.  Tabular data is easier to query from the TabularDataWithStrings table, which is implemented as a database view that joins this table with the Strings table.

Table: SQL TabularData Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ReportNameIndex|INTEGER|A foreign key to an entry in the Strings table, which indicates the name of the report the record belongs to.
ReportForStringIndex|INTEGER|A foreign key to an entry in the Strings table, which indicates the "For" text that is associated with the record.  An example is the Space Loads report where the "For" text identifies which zone the record pertains to.
TableNameIndex|INTEGER|A foreign key to an entry in the Strings table, which indicates the name of the table the record belongs to.  This is typically the text that immediately precedes the tables in html format.
SimulationIndex|INTEGER|A foreign key to the Simulations table.
RowNameIndex|INTEGER|A foreign key to an entry in the Strings table, which indicates the name of the row the record belongs to.
ColumnNameIndex|INTEGER|A foreign key to an entry in the Strings table, which indicates the name of the column the record belongs to.
RowId|INTEGER|The index of the row associated with the record.  Data in the first row of a tabular report would have the Id 1.
ColumnId|INTEGER|The index of the column associated with the record.  Data in the first column of a tabular report would have the Id 1.
Value|REAL|The value of the record.  Most data is numeric, thus the type for this column is REAL.  The SQLite engine will first try to store the value as a REAL, however if this fails the value will be stored as text.
UnitsIndex|INTEGER|A foreign key to an entry in the Strings table, which indicates the units associated with the record.

### Strings Table

An overview of the Strings SQL table is shown below. The Strings table is only output when the "SimpleAndTabular" choice is made in the Output:SQLite object.  The Strings table simply associates strings with integers to save space in the database.

Table: SQL Strings Table Contents

Field Name|Field Type|Description
----------|----------|-----------
StringIndex|INTEGER PRIMARY KEY|The integer primary key that uniquely identifies the string record.
StringTypeIndex|INTEGER|A foreign key to the StringTypes table.
Value|TEXT|The record's string value.

### StringTypes Table

An overview of the StringTypes SQL table is shown below. The StringTypes table is only output when the "SimpleAndTabular" choice is made in the Output:SQLite object.

Table: SQL StringTypes Table Contents

Field Name|Field Type|Description
----------|----------|-----------
StringTypeIndex|INTEGER PRIMARY KEY|The integer primary key.
Value|TEXT|The record's string value.  Currently there are 6 possible values; ReportName, ReportForString, TableName, RowName, ColumnName, and Units.  That is, entries in the Strings table are one of these types.

### TabularDataWithStrings Table

An overview of the TabularDataWithStrings SQL table is shown below.  This table is implemented as a view of other tables and is not actually a table in the database.  This view is similar to the TabularData table except text strings are directly available in place of foreign keys to the Strings table.   The TabularDataWithStrings view is only created when the "SimpleAndTabular" choice is made in the Output:SQLite object.

An example of a SQL query to get a table out of the AnnualBuildingUtilityPerformanceSummary report is the following.

~~~~~~~~~~~~~~~~~~~~

    select * FROM TabularDataWithStrings WHERE ReportName='AnnualBuildingUtilityPerformanceSummary'
    and TableName='Site and Source Energy';
~~~~~~~~~~~~~~~~~~~~

Table: SQL TabularDataWithStrings Table Contents

Field Name|Field Type|Description
----------|----------|-----------
Value|REAL|The value of the record
ReportName|TEXT|The name of the report the record belongs to.
ReportForString|TEXT|The "For" string.
TableName|TEXT|The name of the table the record belongs to.
RowName|TEXT|The name of the row associated with the record.
ColumnName|TEXT|The name of the column associated with the record.
Units|TEXT|The units associated with the record.

### Errors Table

The Errors SQL table reports errors and warnings for the simulation.The content of this table includes most of the content of the error file.  However, it does not contain purely informational messages (e.g. not warnings or errors) and some of the error summary statistics which may be readily computed. Users should be aware that errors experienced while processing input cause EnergyPlus to exit before the SQLite output database has been initialized.  This is because the Output:SQLite object must be parsed from the input to request the database.  Therefore, failures in input processing are indicated by the absence of a SQLite output file.  The regular error or audit files must be used to identify the erroneous input objects in this case.  Because each error or warning message is expected to be unique these messages are stored directly in the Errors table and not in the Strings table.

Table: SQL Errors Table Contents

Field Name|Field Type|Description
----------|----------|-----------
ErrorIndex|INTEGER PRIMARY KEY|The index of the error or warning message.
SimulationIndex|INTEGER FOREIGN KEY|The simulation the error or warning pertains to.
ErrorType|INTEGER|Type of error or warning 0=Warning, 1=Severe, 2=Fatal.
ErrorMessage|TEXT|The text of the error message.
Count|INTEGER|Number of times that the error was repeated.  This will be 1 for normal warnings or errors, 0 for warning and error messages, and the number of times the error or warning is repeated.

## How to Access the SQLite Data

The SQL database can be accessed in a number of ways, including via the command line, through ODBC, or through as SQLite's API interface. SQLite uses the industry standard SQL 92 language.

### Command Line

One of the simplest ways to access the data in the SQL database is by way of the SQL command line tool (i.e., sqlite3). A brief description of how to use sqlite3 for each computing platform is given below.

### Windows XP and Windows Vista

While Windows does not ship with sqlite3 installed, the sqlite3 binary can be downloaded from the SQLite webpage (www.sqlite.org/download.html). After downloading the precompiled binary, install it in the EnergyPlus directory.

Once the sqlite3 executable is installed, access the program from the command line by typing "sqlite3" at the DOS prompt.

### Linux

The sqlite3 command line tool comes preinstalled on a number of more recent Linux releases. To see if sqlite3 is available (and which version is installed), type "sqlite3 --version" from the command line. If sqlite3 is not installed, the sqlite3 binary, as well as source code, can be downloaded from the SQLite webpage (http://www.sqlite.org/download.html) and installed in the directory of your choice.

### Macintosh OS X

The sqlite3 program comes standard on MacOS X 10.5. From the command line, type "sqlite3 --version" to see which version of sqlite3 is installed.  In order to access the database created by EnergyPlus, version 3 or later is required.

### Accessing the Data from the Command Line

Once it has been confirmed that SQLite3 is installed on your machine, the SQL database can be accessed by typing:

~~~~~~~~~~~~~~~~~~~~

    sqlite3 <database name>
~~~~~~~~~~~~~~~~~~~~

at the command line, where <database name > is the name of the SQL database (e.g., sqlite3 eplusout.sql).

The sqlite.org website, http://www.sqlite.org/sqlite.html, gives examples of how sqlite3 can be used to access and output data in various formats.

### ODBC

ODBC allows access to the SQL database from a variety of programs, including Microsoft Excel, Microsoft Access, and FileMaker. How to install and use ODBC drivers is outside the scope of this document, and more information can be found at the following websites:

Macintosh ODBC drivers:

http://www.actualtechnologies.com/

Windows and Linux ODBC information and drivers:

http://www.sqlite.org/cvstrac/wiki?p=SqliteOdbc

http://www.ch-werner.de/sqliteodbc/

### API

Sqlite3 includes a rich C++ API (detailed on the SQLite website www.sqlite.org/cintro.html), and wrappers for the API interface are available in a variety of programming languages, including Fortran, TCL, and Ruby (see www.sqlite.org/cvstrac/wiki?p=SqliteWrappers for more information).