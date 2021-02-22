# Cooling Energy Storage Sizing Pair

**Bo Shen and Jian Sun, ORNL**

- February 14, 2021 - Initial Draft

## Justification for Feature Update
Bluilding energy storages aim to replace a main cooling or heating coil to provide supplemental cooling or heating for a period of time. The main cooling/heating coil should have been sized in its air loop. Thus, the energy storage devices, i.e. ice storage tank or hot water tank should be sized accordingly, which multiplies the rated capacity of a main coil by the intended operation period. EnergyPlus doesn't have this logic yet. This development will introduce a new sizing object to associate a storage device to its main cooling coil. 

## Model Description
The cooling storage sizing pair will define a type and name of a storage object, e.g. ice storage tank, and assiciate it to a main cooling coil. It will also define the maximum operation hours of the storage. A main cooling coil meets sensible and latent cooling loads. The user defines what load the storage device will be sized to meet, including sensible, latent or total. Optionally, a recovery unit can be defined, e.g. a chiller object to charge an ice storage tank. A ratio can be given to associate the recovery unit's rated capacity to the one of the main cooling coil. 

If choose to use the energy storage to meet the total capacity for a given period, 

The storage device's capacity = Main coil's Rated Total Cooling Capacity * Operation Hours

If choose to use the energy storage to meet the sensible capacity for a given period, 

The storage device's capacity = Main coil's Rated Total Cooling Capacity * Main coil's Rated Sensible Heat Ratio * Operation Hours

If choose to use the energy storage to meet the latent capacity for a given period, 

The storage device's capacity = Main coil's Rated Total Cooling Capacity *(1.0 -  Main coil's Rated Sensible Heat Ratio) * Operation Hours

The rated capacity of the recovery unit = Main coil's Rated Total Cooling Capacity * recovery unit ratio to the main cooling coil

## Implementation

The new codes were added to VariableSpeedCoils.cc and VariableSpeedCoils.hh.

## Testing

A new example file has been added to demonstrate the new features. The example file takes the ``5ZoneIceStorage.idf`` file and  modifies it to "5Zone_VSDXCoolIceStorage.idf" which simulated a grid-responsive cooling coil, pairing with a ice storage and a recovery chiller unit.

## Documentation

Some inline code documentation has been added, and additional documentation will be added later.

## IDD Changes and Transition

Input example is shown below. 

  ThermalStorage:Cooling:Pair,

    CoolingStorePair,     !- Name
    Coil:Cooling:DX:VariableSpeed, !- Cooling Coil Object Type
    DX Cooling Coil 1,    !- Cooling Coil Name
    ThermalStorage:Ice:Detailed,  !- Tank Object Type
    Ice Tank,                !- Tank Name   
    4.0,                     !-Maximum Peak Operation Hours
    0.0,                     !-temperature change	
    TOTAL,                   ! - load met type
	Chiller:Electric,        ! - recovery unit type
	Central Chiller,         ! - recovery unit name
	0.5;                     ! - recovery unit ratio to the main cooling coil

    

IDD Objects (New): 

ThermalStorage:Cooling:Pair,

       \min-fields 7
       \memo This object pairs a storage tank with a variable speed DX cooling coil to sustain a few hours of operation
  
     A1,  \field Name
           \type alpha
           \required-field
      A2,  \field Cooling Coil Object Type
           \type choice
           \key Coil:Cooling:DX:VariableSpeed
	       \default Coil:Cooling:DX:VariableSpeed
           \note  Only works with Coil:Cooling:DX:VariableSpeed 
      A3,  \field Cooling Coil Name
           \required-field
           \type object-list
           \object-list CoolingCoilsDXVariableSpeed
           \note  Needs to match in the DX cooling coil object
      A4,  \field Tank Object Type
           \type choice
           \key ThermalStorage:Ice:Detailed
           \key ThermalStorage:Ice:Simple
	       \key ThermalStorage:Pcm:Simple
      A5,  \field Tank Name
           \required-field
           \type object-list
           \object-list IceThermalStorageEquipment 
      N1,  \field Maximum Peak Operation Hours
           \required-field
           \type real
           \units hrs
           \ip-units hrs
      N2,  \field Temperature or Concentration Change in Tank through Operation
           \required-field
	       \default 0.0
      A6,  \field Load Type
           \type choice
           \key TOTAL
           \key LATENT
           \default TOTAL
           \note Select either total load or latent (moisture) load.
      A7,  \field Recovery Unit Type
           \type choice
           \key Chiller:Electric
           \key Chiller:Electric:EIR
      A8,  \field Recovery Unit Name
           \type object-list
           \object-list Chillers
      N3;  \field Capacity Ratio of Recovery Unit to Main Cooling Coil
           \type real
	       \default 1.0