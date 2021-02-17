Input Changes version 9.4.0 to 9.5.0
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: Construction:AirBoundary

Summary: The fields for *Solar and Daylighting Method* and *Radiant Exchange Method* have been removed. All air boundaries will be modeled using the former "GroupedZones" option. The former options for "InteriorWindow" and "IRTSurface" are no longer available and will generate a transition warning.

Field 1 remains the same.
Fields 2 and 3 are deleted.
Fields 4-6 remain the same, shifting up to new fields 2-4.

See [8370](https://github.com/NREL/EnergyPlus/pull/8370)

# Split Object: ‘ZoneHVAC:LowTemperatureRadiant:VariableFlow’

## Object Change: ‘ZoneHVAC:LowTemperatureRadiant:VariableFlow’

Field 1 remain the same.

Insert field 2 - Design Object.

Field 3, previous Field 2 - Availability Schedule Name

Field 4, previous Field 3 -  Zone Name

Field 5, previous Field 4 -  Surface Name or Radiant Surface Group Name

Field 6, previous Field 8 -  Hydronic Tubing Length

Field 7, previous Field 13 - Heating Design Capacity

Field 8, previous Field 16 - Maximum Hot Water Flow

Field 9, previous Field 17 - Heating Water Inlet Node Name

Field 10, previous field 18 - Heating Water Outlet Node Name

Field 11, previous field 22 - Cooling Design Capacity

Field 12, previous field 25 - Maximum Cold Water Flow

Field 13, previous field 26 - Cooling Water Inlet Node Name

Field 14, previous field 27 - Cooling Water Outlet Node Name

Field 15, previous field 32 - Number of Circuits

Field 16, previous field 33 - Circuit Length


## New Object: ‘ZoneHVAC:LowTemperatureRadiant:VariableFlow:Design’

Insert Field 1 - Name.

Field 2, previous Field 5 - Fluid to Radiant Surface Heat Transfer Model

Field 3, previous Field 6 - Hydronic Tubing Inside Diameter

Field 4, previous Field 7 -  Hydronic Tubing Outside Diameter

Field 5, previous Field 9 -  Hydronic Tubing Conductivity

Field 6, previous Field 10 -  Temperature Control Type

Field 7, previous Field 11 - Setpoint Control Type

Field 8, previous Field 12 - Heating Design Capacity Method

Field 9, previous Field 14 - Heating Design Capacity Per Floor Area

Field 10, previous field 15 - Fraction of Autosized Heating Design Capacity

Field 11, previous field 19 - Heating Control Throttling Range

Field 12, previous field 20 - Heating Control Temperature Schedule Name

Field 13, previous field 21 - Cooling Design Capacity Method

Field 14, previous field 23 - Cooling Design Capacity Per Floor Area

Field 15, previous field 24 - Fraction of Autosized Cooling Design Capacity

Field 16, previous field 28 - Cooling Control Throttling Range

Field 17, previous field 29 - Cooling Control Temperature Schedule Name

Field 18, previous field 30 - Condensation Control Type

Field 19, previous field 31 - Condensation Control Dewpoint Offset

Field 20, previous field 34 - Changeover Delay Time Period Schedule

# Split Object: ‘ZoneHVAC:LowTemperatureRadiant:ConstantFlow’

## Object Change: ‘ZoneHVAC:LowTemperatureRadiant:ConstantFlow’

Field 1 remain the same.

Insert field 2 - Design Object.

Field 3, previous Field 2 - Availability Schedule Name

Field 4, previous Field 3 -  Zone Name

Field 5, previous Field 4 -  Surface Name or Radiant Surface Group Name

Field 6, previous Field 8 -  Hydronic Tubing Length

Field 7, previous Field 12 - Rated Flow Rate

Field 8, previous Field 13 - Pump Flow Rate Schedule Name

Field 9, previous Field 14 - Rated Pump Head

Field 10, previous field 15 - Rated Power Consumption

Field 11, previous field 18 - Heating Water Inlet Node Name

Field 12, previous field 19 - Heating Water Outlet Node Name

Field 13, previous field 20 - Heating High Water Temperature Schedule Name

Field 14, previous field 21 - Heating Low Water Temperature Schedule Name

Field 15, previous field 22 - Heating High Control Temperature Schedule Name

Field 16, previous field 23 - Heating Low Control Temperature Schedule Name

Field 17, previous field 24 - Cooling Water Inlet Node Name

Field 18, previous field 25 - Cooling Water Outlet Node Name

Field 19, previous field 26 - Cooling High Water Temperature Schedule Name

Field 20, previous field 27 - Cooling Low Water Temperature Schedule Name

Field 21, previous field 28 - Cooling High Control Temperature Schedule Name

Field 22, previous field 29 - Cooling Low Control Temperature Schedule Name

Field 23, previous field 32 - Number of Circuits

Field 24, previous field 33 - Circuit Length

## New Object: ‘ZoneHVAC:LowTemperatureRadiant:ConstantFlow:Design’

Insert Field 1 - Name.

Field 2, previous Field 5 - Fluid to Radiant Surface Heat Transfer Model

Field 3, previous Field 6 -  Hydronic Tubing Inside Diameter

Field 4, previous Field 7 -  Hydronic Tubing Outside Diameter

Field 5, previous Field 9 -  Hydronic Tubing Conductivity

Field 6, previous Field 10 - Temperature Control Type

Field 7, previous Field 11 -  Running Mean Outdoor Dry-Bulb Temperature Weighting Factor

Field 8, previous Field 16 - Motor Efficiency

Field 9, previous Field 17 - Fraction of Motor Inefficiencies to Fluid Stream

Field 10, previous field 30 - Condensation Control Type

Field 11, previous field 31 - Condensation Control Dewpoint Offset

Field 12, previous field 34 - Changeover Delay Time Period Schedule


# Split Object: ‘ZoneHVAC:Baseboard:RadiantConvective:Water’

## Object Change: ‘ZoneHVAC:Baseboard:RadiantConvective:Water’

Field 1 remain the same.

Insert field 2, Design Object.

Field 3, previous Field 2 - Availability Schedule Name

Field 4, previous Field 3 -  Inlet Node Name

Field 5, previous Field 4 -  Outlet Node Name

Field 6, previous Field 5 -  Rated Average Water Temperature

Field 7, previous Field 6 - Rated Water Mass Flow Rate

Field 8, previous Field 8 - Heating Design Capacity

Field 9, previous Field 11 - Maximum Water Flow Rate

Field 10, previous field 15 - Surface 1 Name

And rest

## New Object: ‘ZoneHVAC:Baseboard:RadiantConvective:Water:Design’

Insert Field 1 - Name.

Field 2, previous Field 7 - Heating Design Capacity Method

Field 3, previous Field 9 - Heating Design Capacity Per Floor Area

Field 4, previous Field 10 -  Fraction of Autosized Heating Design Capacity

Field 5, previous Field 12 -  Convergence Tolerance

Field 6, previous Field 13 -  Fraction Radiant

Field 7, previous Field 14 -  Fraction of Radiant Energy Incident on People

# Split Object: ‘ZoneHVAC:Baseboard:RadiantConvective:Steam’

## Object Change: ‘ZoneHVAC:Baseboard:RadiantConvective:Steam’

Field 1 remain the same.

Insert field 2, Design Object.

Field 3, previous Field 2 - Availability Schedule Name

Field 4, previous Field 3 -  Inlet Node Name

Field 5, previous Field 4 -  Outlet Node Name

Field 6, previous Field 6 - Heating Design Capacity

Field 7, previous Field 9 - Degree of SubCooling

Field 8, previous field 10 - Maximum Steam Flow Rate

Field 9, previous field 14 - Surface 1 Name

And rest

## New Object: ‘ZoneHVAC:Baseboard:RadiantConvective:Steam:Design’

Insert Field 1 - Name.

Field 2, previous Field 5 - Heating Design Capacity Method

Field 3, previous Field 7 - Heating Design Capacity Per Floor Area

Field 4, previous Field 8 -  Fraction of Autosized Heating Design Capacity

Field 5, previous Field 11 -  Convergence Tolerance

Field 6, previous Field 12 -  Fraction Radiant

Field 7, previous Field 13 -  Fraction of Radiant Energy Incident on People
