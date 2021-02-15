# Heating Energy Storage Sizing Pair

**Bo Shen and Jian Sun, ORNL**

- February 14, 2021 - Initial Draft

## Justification for Feature Update
Bluilding energy storages aim to replace a main cooling or heating coil to provide supplemental cooling or heating for a period of time. The main cooling/heating coil should have been sized in its air loop. Thus, the energy storage devices, i.e. ice storage tank or hot water tank should be sized accordingly, which multiplies the rated capacity of a main coil by the intended operation period. EnergyPlus doesn't have this logic yet. This development will introduce a new sizing object to associate a hot water storage tank to its main heating coil. 

## Model Description
The heating storage sizing pair will define a type and name of a storage object, e.g. hot water tank, and assiciate it to a main heating coil. It will also define the maximum operation hours of the storage. Optionally, a recovery unit can be defined, e.g. a heat pump water heater to heat the water storage tank. A ratio can be given to associate the recovery unit's rated capacity to the one of the main heating coil. 

The storage device's capacity = Main coil's Rated Total Heating Capacity * Operation Hours

The rated capacity of the recovery unit = Main coil's Rated Total Heating Capacity * recovery unit ratio to the main heating coil

## Implementation

The new codes were added to VariableSpeedCoils.cc and VariableSpeedCoils.hh.

## Testing

Two new example files have been added to demonstrate the new features. The example file takes the ``HeatPumpVSAS.idf`` file and  modifies it to "IDHeatPumpVSASGridWhStorage.idf"  and "ODHeatPumpVSASGridWhStorage.idf" which simulated a grid-responsive heating coil, pairing with a hot water storage and a recovery heat pump water heater unit.

## Documentation

Some inline code documentation has been added, and additional documentation will be added later.

## IDD Changes and Transition

Input example is shown below. 

  ThermalStorage:Heating:Pair,

    HeatingStorePair,     !- Name
    Coil:Heating:DX:VariableSpeed, !- Heating Coil Object Type
    Heat Pump DX Heating Coil 1,    !- Heating Coil Name
    WaterHeater:Mixed,  !- Tank Object Type
    HPWHZoneTank,                !- Tank Name   
    4.0,                     !-Maximum Peak Operation Hours
    15.0,                     !-temperature change	
	Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed,        ! - recovery unit type
	HPWHVSCoil,         ! - recovery unit name
	1.0;                     ! - recovery unit ratio to the main heating coil

    

IDD Objects (New): 

ThermalStorage:Heating:Pair,

       \min-fields 7
       \memo This object pairs a storage tank with a variable speed DX heating coil to sustain a few hours of operation
  
    A1,  \field Name
        \type alpha
        \required-field
    A2,  \field Heating Coil Object Type
        \type choice
        \key Coil:Heating:DX:VariableSpeed
	    \default Coil:Heating:DX:VariableSpeed
        \note  Only works with Coil:Heating:DX:VariableSpeed 
    A3,  \field Heating Coil Name
        \required-field
        \type object-list
        \object-list HeatingCoilsDXVariableSpeed
        \note  Needs to match in the DX heating coil object
    A4,  \field Tank Object Type
        \type choice
        \key WaterHeater:Mixed
        \key WaterHeater:Stratified
        \default WaterHeater:Mixed
    A5,  \field Tank Name
        \required-field
        \type object-list
        \object-list WaterHeaterMixedNames
        \object-list WaterHeaterStratifiedNames  
    N1,  \field Maximum Peak Operation Hours
        \required-field
        \type real
        \units hrs
        \ip-units hrs
    N2,  \field Temperature Change in Tank through Operation
        \required-field
        \type real
	    \units K
    A6,  \field Recovery Unit Type
        \type choice
        \key COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED
	    \default COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED
    A7,  \field Recovery Unit Name
        \required-field
        \type object-list
        \object-list HeatPumpWaterHeaterDXCoilsVariableSpeed
    N3;  \field Capacity Ratio of Recovery Unit to Main Cooling Coil
        \type real
	    \default 1.0