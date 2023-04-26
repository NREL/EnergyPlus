Extend Spaces to Sizing and HVAC
================

**Michael J. Witte, GARD Analytics, Inc.**

 - Original April 26, 2023

## Table of Contents ##

[Justification for New Feature](#justification-for-new-feature)

[E-mail and Conference Call Conclusions](#e-mail-and-conference-call-conclusions)

[Overiew](#overview)

[Approach](#approach)

[Testing/Validation/Data Sources](#testingvalidationdata-sources)

[Input Description](#input-description)

[Outputs Description](#outputs-description)

[Engineering Reference](#engineering-reference)

[Example File and Transition Changes](#example-file-and-transition-changes)

## Justification for New Feature ##

Space was added as a new concept in v9.6. Each EnergyPlus Zone contains one or more Spaces which are used for:

  * assigning and allocating internal gains
  * specifying enclosure boundaries,
  * reporting inputs grouped by space types, and 
  * reporting select output grouped by space types.
  
For the zone heat balance each thermal Zone is composed of one or more Spaces controlled by a single thermostat or HVAC system control point such as a VAV terminal unit.

In version 23.1 options were added to perform the air heat balance for each Space or each Zone, and space-level heat balance output variables were added.

This NFP proposes additional optional capabilities:

  * Space-level (room-by-room) sizing to size individual room diffusers or equipment such as PTACs. 
  
  * Zone-level equipment may be sized to either the coincident or non-coincident peak across the Spaces (rooms). 

  * Space-by-space HVAC simulation to allow modeling the impact of thermostat placement in a specific space (room). This may result in less-than-ideal temperature control for other spaces in the same Zone. For some applications, such as a unitary system with a single thermostat, this can already be modeled with the "control zone" options. This feature would extend to other system types, including VAV systems or unitary systems with zoning controls.

## E-mail and Conference Call Conclusions ##

n/a

## Overview ##

The `ZoneAirHeatBalanceAlgorithm` object has options for space heat balance.
```
ZoneAirHeatBalanceAlgorithm,
    ThirdOrderBackwardDifference,  !- Algorithm
    Yes,                      !- Do Space Heat Balance for Sizing
    Yes;                      !- Do Space Heat Balance for Simulation
```
These currently result in additional output variables for Space, but nothing more.

### Extend Spaces to Sizing
When "Do Space Heat Balance for Sizing" is active, the following sizing changes will be active:

1. Sizing results will be reported for every Space and Zone.
2. Space sizing will follow the same method currently used for Zone sizing.
3. Zone sizing will be based on coincident or non-coincident Space sizing results.
4. Zone sizing results will be used to size air loops.
5. Space sizing results will be used to size space-level HVAC if specified.


### Extend Spaces to HVAC
Inputs will be added to describe optional Space-level HVAC equipment, such as:

1. Space air terminal units supplied by a single zone-level air distribution unit. e.g. A zone-level VAV damper supplying multiple space diffusers.
2. Self-contained zone equipment, such as PTACs and fancoil units may be assigned to a Space. Maybe?
3. Thermostats will still control a Zone, but they may be placed in a specific Space and will control that Space's temperature.
4. Finish adding Space Name options to various features such as ZoneThermalChimney (if budget allows, otherwise restrict these to single-space zones).


## Approach ##


## Testing/Validation/Data Sources ##

Compare Space vs Zone-level results.

## Input Description ##
Some new objects and some changes to existing objects are proposed.

### ZoneControl:Thermostat:\*
* *Change field "Zone or ZoneList Name" to "Zone or ZoneList or Space or SpaceList Name."*
* When a thermostat is assigned to a zone, it will calculate the lumped loads to setpoint for all spaces in the zone.
* When a thermostat is assigned to a space, it will calculate the loads to setpoint for that space.


### SpaceHVAC:EquipmentConnections
* *New object to connect nodes to a Space, similar to ZoneHVAC:EquipmentConnections.*

```
SpaceHVAC:EquipmentConnections,
       \memo Specifies the HVAC equipment connections for a Space. Node names are specified for the
       \memo Space air node and air inlet nodes.
  A1 , \field Space Name
       \required-field
       \type object-list
       \object-list SpaceNames
  A2 , \field Space Air Node Name
       \required-field
       \type node
  A3 ; \field Space Air Inlet Node or NodeList Name
       \type node
```

### ZoneHVAC:SpaceSplitter
* *New object, splits the air flow from a single piece of zone equipment to one or more Spaces.*
```
ZoneHVAC:SpaceSplitter,
       \extensible:2
       \memo Split one air stream into N outlet streams. Node names
       \memo Node names must be unique across all ZoneHVAC:SpaceSplitter objects.
   A1, \field Name
       \required-field
       \reference SpaceSplitters
   A2, \field Inlet Node Name
       \required-field
       \type node
   A3, \field Outlet 1 Node Name
       \begin-extensible
       \required-field
       \type node
   N1, \field Outlet 1 Flow Fraction
       \units dimensionless
       \type real
       \minimum 0
       \maximum 1.0
       \autosizable
       \default autosize
   A4, \field Outlet 2 Node Name
       \required-field
       \type node
   N2, \field Outlet 2 Flow Fraction
       \units dimensionless
       \type real
       \minimum 0
       \maximum 1.0
       \autosizable
       \default autosize
```

### ZoneHVAC:AirDistributionUnit
* *Add new field.*

  A6 ; \field Space Splitter Name
       \type object-list
       \object-list SpaceSplitters

### ZoneRefrigerationDoorMixing
(If budget allows, otherwise limit these to single-space zones.)
* *Change field "Zone 1 Name" to "Zone or Space Name 1."*

* *Change field "Zone 2 Name" to "Zone or Space Name 2."*

### ZoneCoolTower:Shower
(If budget allows, otherwise limit these to single-space zones.)
* *Change field "Zone Name" to "Zone or Space Name."*

### ZoneThermalChimney
(If budget allows, otherwise limit these to single-space zones.)
* *Change field "Zone N Name" to "Inlet Zone or Space Name N."*

## Outputs Description ##
Existing relevant Space Output Variables (from 5ZoneAirCooledWithSpaceHeatBalance.rdd)
```
Output:Variable,*,Space Mean Air Temperature,hourly; !- Zone Average [C]
Output:Variable,*,Space Operative Temperature,hourly; !- Zone Average [C]
Output:Variable,*,Space Mean Air Dewpoint Temperature,hourly; !- Zone Average [C]
Output:Variable,*,Space Mean Air Humidity Ratio,hourly; !- Zone Average [kgWater/kgDryAir]
Output:Variable,*,Space Air Heat Balance Internal Convective Heat Gain Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space Air Heat Balance Surface Convection Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space Air Heat Balance Interzone Air Transfer Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space Air Heat Balance Outdoor Air Transfer Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space Air Heat Balance System Air Transfer Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space Air Heat Balance System Convective Heat Gain Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space Air Heat Balance Air Energy Storage Rate,hourly; !- HVAC Average [W]

Output:Variable,*,Space Air Temperature,hourly; !- HVAC Average [C]
Output:Variable,*,Space Air Humidity Ratio,hourly; !- HVAC Average []
Output:Variable,*,Space Air Relative Humidity,hourly; !- HVAC Average [%]

Output:Variable,*,Space Air System Sensible Heating Energy,hourly; !- HVAC Sum [J]
Output:Variable,*,Space Air System Sensible Cooling Energy,hourly; !- HVAC Sum [J]
Output:Variable,*,Space Air System Sensible Heating Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space Air System Sensible Cooling Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space Predicted Sensible Load to Setpoint Heat Transfer Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space Predicted Sensible Load to Heating Setpoint Heat Transfer Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space System Predicted Sensible Load to Setpoint Heat Transfer Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space System Predicted Sensible Load to Heating Setpoint Heat Transfer Rate,hourly; !- HVAC Average [W]
Output:Variable,*,Space System Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate,hourly; !- HVAC Average [W]

Output:Variable,*,Space Predicted Moisture Load Moisture Transfer Rate,hourly; !- HVAC Average [kgWater/s]
Output:Variable,*,Space Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate,hourly; !- HVAC Average [kgWater/s]
Output:Variable,*,Space Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate,hourly; !- HVAC Average [kgWater/s]
Output:Variable,*,Space System Predicted Moisture Load Moisture Transfer Rate,hourly; !- HVAC Average [kgWater/s]
Output:Variable,*,Space System Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate,hourly; !- HVAC Average [kgWater/s]
Output:Variable,*,Space System Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate,hourly; !- HVAC Average [kgWater/s]
```

*Space equivalents may be added for the following zone output variables (not sure about some of these):*

```
Zone,Average,Zone Mean Radiant Temperature [C]

HVAC,Sum,Zone Infiltration Sensible Heat Loss Energy [J]
HVAC,Sum,Zone Infiltration Sensible Heat Gain Energy [J]
HVAC,Sum,Zone Infiltration Latent Heat Loss Energy [J]
HVAC,Sum,Zone Infiltration Latent Heat Gain Energy [J]
HVAC,Sum,Zone Infiltration Total Heat Loss Energy [J]
HVAC,Sum,Zone Infiltration Total Heat Gain Energy [J]
HVAC,Average,Zone Infiltration Current Density Volume Flow Rate [m3/s]
HVAC,Average,Zone Infiltration Standard Density Volume Flow Rate [m3/s]
HVAC,Sum,Zone Infiltration Current Density Volume [m3]
HVAC,Sum,Zone Infiltration Standard Density Volume [m3]
HVAC,Sum,Zone Infiltration Mass [kg]
HVAC,Average,Zone Infiltration Mass Flow Rate [kg/s]
HVAC,Average,Zone Infiltration Air Change Rate [ach]

HVAC,Average,Zone Thermostat Air Temperature [C]
Zone,Average,Zone Thermostat Control Type []
HVAC,Average,Zone Thermostat Heating Setpoint Temperature [C]
HVAC,Average,Zone Thermostat Cooling Setpoint Temperature [C]
Zone,Average,Zone Adaptive Comfort Operative Temperature Set Point [C]
HVAC,Average,Zone Predicted Sensible Load Room Air Correction Factor []

HVAC,Sum,Zone Oscillating Temperatures Time [hr]
HVAC,Sum,Zone Oscillating Temperatures During Occupancy Time [hr]
HVAC,Sum,Zone Oscillating Temperatures in Deadband Time [hr]
HVAC,Sum,Facility Any Zone Oscillating Temperatures Time [hr]
HVAC,Sum,Facility Any Zone Oscillating Temperatures During Occupancy Time [hr]
HVAC,Sum,Facility Any Zone Oscillating Temperatures in Deadband Time [hr]
Zone,Sum,Zone Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time [hr]
Zone,Sum,Zone Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time [hr]
Zone,Sum,Zone Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time [hr]

Zone,Sum,Zone Heating Setpoint Not Met Time [hr]
Zone,Sum,Zone Heating Setpoint Not Met While Occupied Time [hr]
Zone,Sum,Zone Cooling Setpoint Not Met Time [hr]
Zone,Sum,Zone Cooling Setpoint Not Met While Occupied Time [hr]
```

## Engineering Reference ##

Summary paragraphs or sentences will be added to indicate that references to "Zone" throughout the heat balance documentation are applicable to "Space or Zone".

## Example File and Transition Changes ##

* No transition will be required for idf inputs.

* Field name changes will be required for epJSON inputs.

* The existing example file 5ZoneAirCooledWithSpaces will extended to add Space HVAC objects.

