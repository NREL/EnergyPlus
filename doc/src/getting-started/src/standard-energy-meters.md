# Standard Energy Meters

Meters provide one way for EnergyPlus to report energy use in a form that is pallatable to the users. The primary implemented method for output gives very fine detail (down to the variable level) for results from EnergyPlus. However, to get the required energy use, there may be several variables that need to be polled and accumulated. The meter implementation for EnergyPlus accomplishes this reporting.

![Illustration of Energy Metering](media/illustration-of-energy-metering.jpeg)


Meters can be used to typify energy use by type and by component. The diagrams and tables illustrate how the meters have been incorporated into EnergyPlus.

As shown in the figure above, energy use for the facility is grouped according to fuel type (see Table 8. Table of Metered Fuel Types), by meter type (see Table 7. Overall Meter Types) and by end use category type (see Table 9. End Use Category Types).

Table: Overall Meter Types

Meters
------
Facility
Building
Zone
System
Plant

Both the fuel types and enduse types are set within the program by the developers. Current Fuel types are shown in the table below. There is also a special category called "EnergyTranser".

Table: Table of Metered Fuel Types

**Utility/Fuel Types**
-----------------------------------
Electricity|Gas
Gasoline|Diesel
Coal|FuelOil#1
FuelOil#2|Propane
Water|Steam
DistrictCooling|DistrictHeating

Other Resource Types
--------------------
EnergyTransfer

The end use types are shown in the following table:

Table: End Use Category Types

**End Use Types**
------------------------------
InteriorLights|ExteriorLights
InteriorEquipment|ExteriorEquipment
Fans|Pumps
Heating|Cooling
HeatRejection|Humidifier
HeatRecovery|DHW
Cogeneration|Refrigeration
Miscellaneous|

**Additional End Use Types Only Used for EnergyTransfer**
----------------------------------------------------------------------
HeatingCoils|CoolingCoils
Chillers|Boilers
Baseboard|HeatRecoveryForCooling
HeatReoveryFor Heating|