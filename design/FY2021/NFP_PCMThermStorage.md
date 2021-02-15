# Phase Change Material (PCM) Thermal Storage

**Jian Sun and Bo Shen, ORNL**

- February 14, 2021 - Initial Draft

## Justification for Feature Update
Various building thermal storage technologies have been integrated into grid responsive control strategies in building flexibility studies. There is increase need for E+ to model and simulation these thermal energy storage technologies and integrate them into system and control simulation to evaluate the building equipment flexibility. The present E+ can simulate ice thermal storage and hot water thermal storage, but lack of simulation model for phase change material (PCM) thermal storage. E+ needs to add the features of PCM thermal storage. 

## Overview
A simple PCM thermal energy storage model (object name: ThermalStorage:Pcm:Simple) is developed to simulate the performance of an PCM storage tank. The model leverages the modeling structure of simple ice storage model (object name: ThermalStorage:Ice:Simple). 

This PCM thermal storage model is based on a simple simulation of an PCM storage tank with given capacity. The tank is charged when PCM changing to solid state on the outside of tubes carrying the brine or glycol solution from chiller, and it is discharged when PCM melting down to liquid state. The PCM storage model includes an implied 3-way valve to control the amount if charge/discharge based on the incoming water temperature and the outlet node setpoint temperature. The tank is controlled using the PlantEquipmentOperation:ComponentSetpoint plant operation scheme, and requires that a setpoint be placed by a setpoint manager on the PCM storage Plant Outlet Node. The model currently does not calculate any tank losses. Different from ice thermal storage, the PCM thermal storage has temperature changes during phase change period. The onset temperature of phase change represents the temperature when the PCM starts to melt. The finish temperature of phase change represents the temperature when the PCM completely melts. A linear interpolation is used to calculate the PCM temperature during the phase change period. For this simple PCM thermal energy storage model, these two temperatures (the onset and finish temperatures of phase change) are given from inputs. If no inputs (zero value) are provided, default values will be assigned. Similarly, overall heat transfer coefficients are specified at PCM solid and liquid states. A linear interpolation is used to calculate the overall heat transfer coefficient during the phase change period. For this simple PCM thermal energy storage model, these two overall heat transfer coefficients under solid and liquid state are given from inputs. If no inputs (zero value) are provided, default values will be assigned


## Model Description

ThermalStorage:Pcm:Simple

The models (object name ThermalStorage:Pcm:Simple) simulate performances of a PCM thermal storage. The details of this object is described as following, 

The ThermalStorage:Pcm:Simple object is modeled in a manner similar to the  ThermalStorage:Ice:Simple, and leverage its modeling structure. But this Pcm thermal storage use NTU- ε method to calculate the heat transfer rate between Pcm and fluid of each time step of charge and discharge period, in steady of using a cure-fitting empirical function in ThermalStorage:Ice:Simple. 

Initialization

The storage tank is assumed to be fully charged (full of solid state PCM) at the beginning of each environment. The tank is then allowed to charge and discharge during the warmup days of the environment.

Control

The PCM storage module is a passive component and will accept any flow rate placed on the inlet node. The load on the PCM storage tank is determined by the inlet node flow rate, the inlet node temperature, and the outlet node setpoint temperature. A positive load indicates a request for cooling and the tank discharges if possible. A negative load indicates a request for charging the tank if possible. A zero load indicates the tank is dormant in which case all flow bypasses the tank and the outlet node temperature is set to the inlet node temperature. The model includes an implied 3-way valve which controls the leaving water temperature to the outlet node setpoint temperature if possible.

Charging

When charging is requested, the following limits are calculated to determine the actual charging rate:
1. If the entering water temperature is greater than -1◦C, the charging rate is zero.
2. If the entering water temperature is greater than or equal to the outlet setpoint temperature, the charging rate is zero.
3. If the current fraction of solid PCM stored is 1.0, the tank is fully charge, and the charging rate is zero.
4. If the requested charging rate times the current system timestep is more than the remaining uncharged storage capacity, the charging rate is limited to that required to completely fill the tank.
5. The maximum charging rate which the chilled water flow can provide is determined by the entering water temperature and flow rate and an outlet temperature which is the minimum of -1◦C or the outlet setpoint temperature.
6. The charging rate which the tank can accept is calculated based on NTU- ε method by the following equations:

![PCMcharging](.\HVACFlexFigures\PCMcharging.PNG)


Discharging

When discharging is requested, the following limits are calculated to determine the actual charging rate:
1. If the entering water temperature is less than 1◦C, the discharge rate is zero.
2. If the entering water temperature is less than or equal to the outlet setpoint temperature, the discharge rate is zero.
3. If the current fraction of PCM liquid state stored is 0.0, the tank is fully discharged, and the discharge rate is zero.
4. If the requested discharge rate times the current system timestep is more than the remaining charged storage capacity, the discharge rate is limited to that required to completely deplete the solid PCM in the tank.
5. The maximum discharge rate which the chilled water flow can accept is determined by the entering water temperature and flow rate and an outlet temperature which is the maximum of 1◦C or the outlet setpoint temperature.
6. Similar as charge process, the discharging rate which the tank can accept is calculated based on NTU- ε method by the following equations,
![PCMdischarging](.\HVACFlexFigures\PCMdischarging.PNG)

## Testing

One new example file has been added to demonstrate the new features. The example file takes the ``5ZoneIceStorage.idf`` file and  modifies it to "5ZonePcmStorage.idf". 

## Documentation

Some inline code documentation has been added, and additional documentation will be added later.

## IDD Changes and Transition


Input example is shown below. 
ThermalStorage:Pcm:Simple,

    Pcm Tank,                !- Name
    IceOnCoilInternal,       !- Pcm Storage Type
    0.5,                     !- Capacity {GJ}
    Pcm Tank Inlet Node,     !- Inlet Node Name
    Pcm Tank Outlet Node,    !- Outlet Node Name
    5.5,                     !- Onset temperature of phase change
    7.0,                     !- Finish temperature of phase change
    20000,                   !- Onset UA of phase change material
    20000;                   !- Finish UA of phase change material




IDD Objects (New):

ThermalStorage:Pcm:Simple,

      \min-fields 5
      \memo This pcm storage model is a simplified model
      \memo It requires a setpoint placed on the Chilled Water Side Outlet Node
      \memo It should be placed in the chilled water supply side outlet branch
      \memo followed by a pipe.
      \memo Use the PlantEquipmentOperation:ComponentSetpoint plant operation scheme.

    A1, \field Name
        \type alpha
        \required-field
        \reference IceThermalStorageEquipment
        \reference-class-name validBranchEquipmentTypes
        \reference validBranchEquipmentNames
    A2, \field Ice Storage Type
        \note IceOnCoilInternal = Ice-on-Coil, internal melt
        \note IceOnCoilExternal = Ice-on-Coil, external melt
        \type choice
        \required-field
        \key IceOnCoilInternal
        \key IceOnCoilExternal
    N1, \field Capacity
        \required-field
        \type real
        \units GJ
        \ip-units ton-hrs
    A3, \field Inlet Node Name
        \type node
        \required-field
    A4, \field Outlet Node Name
        \type node
        \required-field
    N2, \field Onset Temperature of phase change
        \type real
        \units deltaC
        \note This input field is optional. If specified, it is used for replace the default value.
        \note If blank or omitted, the default value specified 
	    \default 5.5
    N3, \field Finish Temperature of phase change
        \type real
        \units deltaC
        \note This input field is optional. If specified, it is used for replace the default value.
        \note If blank or omitted, the default value specified 
	    \default 7.0
    N4, \field UA at soild phase of phase change material
        \type real
        \units W/K
        \note This input field is optional. If specified, it is used for replace the default value.
        \note If blank or omitted, the default value specified 
	    \default 20000
    N5; \field UA at liquid phase of phase change material
        \type real
        \units W/K
        \note This input field is optional. If specified, it is used for replace the default value.
        \note If blank or omitted, the default value specified 
	    \default 20000    
