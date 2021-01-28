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

# Object Change: Coil:Heating:WaterToAirHeatPump:EquationFit

Summary: The fields for *Heating Capacity Coefficient 1-5* and *Heating Power Consumption Coefficient 1-5* have been replaced by *Heating Capacity Curve Name* and *Heating Power Consumption Coefficient Curve Name*. Now two quad-linear curves are referenced using these two curve names. 

Field 1-9 remains the same.

Fields 10-14 are replaced by a new field 10 where a curve name is specified. A new quad-linear curve is created with coefficients copied over from old field 10-14. 

Fields 15-19 are replaced by a new field 11 where a curve name is specified. A new quad-linear curve is created with coefficients copied over from old field 15-19. 

Other fields remain the same.

See [8464](https://github.com/NREL/EnergyPlus/pull/8464)

# Object Change: Coil:Cooling:WaterToAirHeatPump:EquationFit

Summary: The fields for *Total Cooling Capacity Coefficient 1-5*, *Sensible Cooling Capacity Coefficient 1-6* and *Cooling Power Consumption Coefficient 1-5* have been replaced by *Total Cooling Capacity Curve Name*, *Sensible Cooling Capacity Curve Name* and *Cooling Power Consumption Coefficient*. 
Now two quad-linear curves and one quint-linear curve are referenced using these three curve names. 

Field 1-10 remains the same.

Fields 11-15 are replaced by a new field 11 where a curve name is specified. A new quad-linear curve is created with coefficients copied over from old field 11-15. 

Fields 16-21 are replaced by a new field 12 where a curve name is specified. A new quint-linear curve is created with coefficients copied over from old field 16-21. 

Fields 22-26 are replaced by a new field 13 where a curve name is specified. A new quad-linear curve is created with coefficients copied over from old field 22-26. 

Other fields remain the same.

See [8464](https://github.com/NREL/EnergyPlus/pull/8464)

# Object Change: HeatPump:WaterToWater:EquationFit:Heating

Summary: The fields for *Heating Capacity Coefficient 1-5* and *Heating Compressor Power Coefficient 1-5* have been replaced by *Heating Capacity Curve Name* and *Heating Compressor Power Coefficient Curve Name*. Now two quad-linear curves are referenced using these two curve names. 

Field 1-9 remains the same.

Fields 10-14 are replaced by a new field 10 where a curve name is specified. A new quad-linear curve is created with coefficients copied over from old field 10-14. 

Fields 15-19 are replaced by a new field 11 where a curve name is specified. A new quad-linear curve is created with coefficients copied over from old field 15-19. 

Other fields remain the same.

See [8464](https://github.com/NREL/EnergyPlus/pull/8464)

# Object Change: HeatPump:WaterToWater:EquationFit:Cooling

Summary: The fields for *Cooling Capacity Coefficient 1-5* and *Cooling Compressor Power Coefficient 1-5* have been replaced by *Cooling Capacity Curve Name* and *Cooling Compressor Power Coefficient Curve Name*. Now two quad-linear curves are referenced using these two curve names. 

Field 1-9 remains the same.

Fields 10-14 are replaced by a new field 10 where a curve name is specified. A new quad-linear curve is created with coefficients copied over from old field 10-14. 

Fields 15-19 are replaced by a new field 11 where a curve name is specified. A new quad-linear curve is created with coefficients copied over from old field 15-19. 

Other fields remain the same.

See [8464](https://github.com/NREL/EnergyPlus/pull/8464)