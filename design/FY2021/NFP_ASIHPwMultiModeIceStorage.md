# Grid Responsive Air-Source Integrated Heat Pump Having Dual-Mode Cooling and Heating Coils, Ice Storage and Chilled Water Coil

**Bo Shen and Jian Sun, ORNL**

- February 14, 2021 - Initial Draft

## Justification for Feature Update
To assess grid-responsive HVAC flexible measures, we need to simulate modulating coils which accept grid signals to restrict its capacity or shut off. This feature has been implemented in the relevant variable-speed cooling and heating coil objects. On the other hand, during a grid-responsive period, the coil with reduced capacity may operate differently from its normal mode, for example, reduce the air flow ratio relative to its delivered capacity, and thus, the moisture removal may be increased to improve the comfort level, when the zone sensible load can't be met by the reduced capacity. We need to incoporate multi-mode cooling and heating coils with capacity modulation. 

EnergyPlus has a single-speed DX cooling coil with ice storage capability. But, it doesn't contain any variable speed coils, i.e. more universal coil objects. it built all the features into one object, while didn't use other EnergyPlus existing features, e.g. ice storage tank, hydronic cooling coil. This development will implement a parent object with a modular structure to manage existing EnergyPlus features for ice storage and delivery. 

Air-source integrated heat pump has already been a collection of coil objects to simulate a multi-functional unit, capable of space cooling, space heating and water heating. We will expand the integrated heat pump object to hold dual-mode cooling and heating coils with ice storage. 
 

## Overview
The latest technology for commercial air conditioners and air-to-air heat pumps can utilize a variable-speed compressor with a variable-speed indoor blower and outdoor fan. Integrated heat pumps (IHP) are multi-functional units capable of space conditioning and water heating. They use condenser waste heat for water heating, and thus achieve significant energy saving. An IHP comprises up to six working modes in total:  (1) space cooling (SC), (2) space heating (SH), (3) dedicated water heating (DWH), (4) combined space cooling and water heating with full condensing (SCWH), (5) combined space cooling and water heating with desuperheating (SCDWH), and (6) combined space heating and water heating with desuperheating (SHDWH). The SC mode has the same operation as the object of Coil:Cooling:DX:VariableSpeed. The SH mode has the same operation as the object of Coil:Heating:DX:VariableSpeed. The DWH mode uses outdoor air as the heating source, which can be represented by an object of Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed. The SCWH mode uses indoor air as the heating source and full condenser heat for water heating, which can be simulated using an object of Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed. 
Certainly, these operation modes can be optional. In essence, the IHP object is a collection of coils. The minimum required coil object is its main cooling coil, and then, the IHP will operate like a single varable-speed DX coil. 

In this work, we will expand the IHP object to comprise a grid responsive cooling mode, an enhanced dehumidification mode, and a grid-responsive heating mode, in addition to its main cooling and heating coils. 

Additionally, the IHP object will serve as a parent object to manage an ice storage tank and a chilled water coil to charge and discharge the storage. 

The parent object will also conduct size calculations for all its children objects. 

## Model Description
The expanded IHP object will contain three variable-speed cooling coils, i.e. Coil:Cooling:DX:VariableSpeed, including a main cooling coil, an enhanced dehumidification coil and another cooling coil which operates during  a grid-response period.  

During cooling mode, if a grid-response cooling coil object exists and indicates a request of grid-response, i.e. a grid signal falls within a range preventing the normal operation, the IHP will switch to its grid-response cooling coil, which host different sets of performance curves, capacities, efficiencies and air flow ratios than the main cooling coil. 
If the grid-responsive coil is not responsive, but there is a call to remove latent load, e.g. a ZoneControl:Humidistat requires moisture removal, the enhanced dehumidification cooling coil will be called. If the enhanced dehumidfication coil doesn't exist, the IHP will call its main cooling coil. 
The logic and association of an IHP containing multiple variable-speed cooling coils are given below: 
![MultiModeCoil](.\HVACFlexFigures\MultiModeCoil.png)

The IHP will comprise two variable-speed heating coils, i.e. Coil:Heating:DX:VariableSpeed, including a main heating coils and a grid-response heating coil. 

During cooling mode, if a grid-response heating coil object exists and indicates a request of grid-response, i.e. a grid signal falls within a range preventing the normal heating, the IHP will call its grid-response heating coil, which host different sets of performance curves, capacities, heating efficiencies and air flow ratios than the main heating coil. If the grid-response heating coil doesn't exist or respond, the IHP object will call its main heating coil. 

The IHP object will expand to include a variable-speed, air-source chiller, which will charge a ice storage tank defined in the IHP. The chiller can use the same compressor as the main cooling/heating coil, at different time; or a separate vapor compression unit that will operate on its own. The operating speed of the chiller is a user input. 

The IHP object defines a storage tank, ice or phase change material (PCM), and a fraction below which the charging operation starts until the tank is fully charged. Additionally, the IHP object accepts inputs of a starting chiller entering temperature when the ice/PCM fraction is zero, and a curve object to correlate the temperature offset from the starting temperature as a function of the tank fraction. 
The logic of an ice storage integrated heat pump is depicted below, 
![IceStorageIHP](.\HVACFlexFigures\IceStorageIHP.png)

Chiller Entering Coolant Temperature = Starting Entering Coolant Temperature - (a+b×frac<sub>ice</sub>+c×frac<sub>ice</sub><sup>2</sup> +d×frac<sub>ice</sub><sup>3</sup>)

where, 
frac<sub>ice</sub> = ice/PCM mass fraction in the storage tank

a-d = regression curve-fit coefficients

The curve can be absent, and then, the offset is always zero. 

The IHP includes a chilled water coil object. The chilled water coil will be turned on when the grid-response cooling coil or the main cooling coil indicates that an operation with reduced electric power is required; otherwise, the chilled water coil is off. 
The chilled water coil obtains its energy from the ice/PCM storage tank. 
It should be noted that a user will also need to define the storage tank and the chilled water coil in a plant loop at the delivery side. The IHP has already created the generation side, and so, there is no need to create the other half plant loop for the chiller charging part. 

All the water heating functions and controls will be the same, containing  dedicated water heating (DWH), combined space cooling and water heating with full condensing (SCWH), combined space cooling and water heating with desuperheating (SCDWH), and combined space heating and water heating with desuperheating (SHDWH).

CoilSystem:IntegratedHeatPump:AirSource Sizing: 

The IHP will size all its children object, by inputting ratios of capacity and flow rates relative to its main cooling coil. 

## Implementation
Modifications to CoilSystem:IntegratedHeatPump:AirSource were implemented to IntegratedHeatPump.cc and IntegratedHeatPump.hh. 
The IHP object can be called from AirLoopHVAC:UnitaryHeatPump:AirToAir, CoilSystem:Cooling:DX and CoilSystem:Heating:DX. 

## Testing

Two new example files has been added to demonstrate the new features. The example file takes the prototye middle office building example in Atlanta file and  modifies it to "IHPGridAC.idf" to simulate dual-mode cooling coils for grid responsive control; 
and "IHPIceStorage.idf" which simulates ice storage and chilled water coil to supplement cooling for a grid responsive control. 

## Documentation

Some inline code documentation has been added, and additional documentation will be added later.

## IDD Changes and Transition

Input example for dual-mode cooling coils is shown below. 

  CoilSystem:IntegratedHeatPump:AirSource,

    PACU_VAV_bot_CoolC DXCoil,                  !- Name
    ,!- Supply Hot Water Flow Sensor Node Name
    Heat Pump ACDXCoil 1,    !- Space Cooling Coil  Name
    Heat Pump DX Heating Coil 1,  !- Space Heating Coil Name
    ,     !- Dedicated Water Heating Coil Name
    ,               !- SCWH Coil Name
    ,          !- SCDWH Cooling Coil Name
    ,            !- SCDWH Water Heating Coil Name
    ,          !- SHDWH Heating Coil Name
    ,            !- SHDWH Water Heating Coil Name
	,						 ! - Enhanced dehumidification coil name
	GridCoolingCoil,						 ! - Grid Response Cooling coil name 
	,						 ! - Grid Response Heating coil name
    23.0,                    !- Indoor Temperature Limit for SCWH Mode {C}
    28.0,                    !- Ambient Temperature Limit for SCWH Mode {C}
    20.0,                    !- Indoor Temperature above Which WH has Higher Priority {C}
    16.0,                    !- Ambient Temperature above Which WH has Higher Priority {C}
    0,                       !- Flag to Indicate Load Control in SCWH Mode {dimensionless}
    1,                       !- Minimum Speed Level for SCWH Mode {dimensionless}
    3.0,                     !- Maximum Water Flow Volume before Switching from SCDWH to SCWH Mode {m3}
    1,                       !- Minimum Speed Level for SCDWH Mode {dimensionless}
    600,                     !- Maximum Running Time before Allowing Electric Resistance Heat Use during SHDWH Mode {s}
    1,                       !- Minimum Speed Level for SHDWH Mode {dimensionless}
	1.0,                     ! - sizing ratio of space heating coil to space cooling coil
	1.0,                     !- sizing ratio of dedicated water heating capacity to space cooling coil
	1.0, 					 !- sizing ratio of combined space cooling and water heating with full condensing - water heating capacity to space cooling coil
	1.0,                     !- sizing ratio of combined space cooling and water heating with desuperheating - space cooling capacity to space cooling coil
	0.15,                    !- sizing ratio of combined space cooling and water heating with desuperheating - water heating capacity to space cooling coil
    1.0,                     !- sizing ratio of combined space heating and water heating with desuperheating - space heating capacity to space cooling coil
	0.15,                    !- sizing ratio of combined space heating and water heating with desuperheating - water heating capacity to space cooling coil
	1.0,                     ! - sizing ratio of enhanced dehumidification coil to space cooling coil
	1.0,                     ! - sizing ratio of grid response cooling coil to space cooling coil
	1.0,					 ! - sizing ratio of grid response heating coil to space cooling coil
	,			 ! - chilled water coil
	,					 ! - chiller is from a single unit or not
	,					    ! - chiller operating speed 
	,					 ! - chiller sizing ratio to the main cooling coil
	,      ! - supplemental cooling coil type
	,        !- supplemental cooling coil name
	,						!-chilled water coil air flow ratio to the main cooling coil, i.e. space cooling mode
	,						!-chilled water coil water flow ratio to the chiller water coil
	,  ! - ice storage tank type
	,                !- Ice storage tank Name
	;					 ! - ice fraction below which starts charging

Input example for ice storage and supplemental chilled water cooling is shown below. 

  CoilSystem:IntegratedHeatPump:AirSource,

    PACU_VAV_bot_CoolC DXCoil,                  !- Name
    ,!- Supply Hot Water Flow Sensor Node Name
    Heat Pump ACDXCoil 1,    !- Space Cooling Coil  Name
    Heat Pump DX Heating Coil 1,  !- Space Heating Coil Name
    ,     !- Dedicated Water Heating Coil Name
    ,               !- SCWH Coil Name
    ,          !- SCDWH Cooling Coil Name
    ,            !- SCDWH Water Heating Coil Name
    ,          !- SHDWH Heating Coil Name
    ,            !- SHDWH Water Heating Coil Name
	,						 ! - Enhanced dehumidification coil name
	,						 ! - Grid Response Cooling coil name 
	,						 ! - Grid Response Heating coil name
    23.0,                    !- Indoor Temperature Limit for SCWH Mode {C}
    28.0,                    !- Ambient Temperature Limit for SCWH Mode {C}
    20.0,                    !- Indoor Temperature above Which WH has Higher Priority {C}
    16.0,                    !- Ambient Temperature above Which WH has Higher Priority {C}
    0,                       !- Flag to Indicate Load Control in SCWH Mode {dimensionless}
    1,                       !- Minimum Speed Level for SCWH Mode {dimensionless}
    3.0,                     !- Maximum Water Flow Volume before Switching from SCDWH to SCWH Mode {m3}
    1,                       !- Minimum Speed Level for SCDWH Mode {dimensionless}
    600,                     !- Maximum Running Time before Allowing Electric Resistance Heat Use during SHDWH Mode {s}
    1,                       !- Minimum Speed Level for SHDWH Mode {dimensionless}
	1.0,                     ! - sizing ratio of space heating coil to space cooling coil
	1.0,                     !- sizing ratio of dedicated water heating capacity to space cooling coil
	1.0, 					 !- sizing ratio of combined space cooling and water heating with full condensing - water heating capacity to space cooling coil
	1.0,                     !- sizing ratio of combined space cooling and water heating with desuperheating - space cooling capacity to space cooling coil
	0.15,                    !- sizing ratio of combined space cooling and water heating with desuperheating - water heating capacity to space cooling coil
    1.0,                     !- sizing ratio of combined space heating and water heating with desuperheating - space heating capacity to space cooling coil
	0.15,                    !- sizing ratio of combined space heating and water heating with desuperheating - water heating capacity to space cooling coil
	1.0,                     ! - sizing ratio of enhanced dehumidification coil to space cooling coil
	1.0,                     ! - sizing ratio of grid response cooling coil to space cooling coil
	1.0,					 ! - sizing ratio of grid response heating coil to space cooling coil
	VSChillerCoil,			 ! - chilled water coil
	SEPARATE,					 ! - chiller is from a single unit or not
	1,					    ! - chiller operating speed 
	1.0,					 ! - chiller sizing ratio to the main cooling coil
	Coil:Cooling:Water,      ! - supplemental cooling coil type
	Main Cooling Coil 1,        !- supplemental cooling coil name
	1.0,						!-chilled water coil air flow ratio to the main cooling coil, i.e. space cooling mode
	1.0,						!-chilled water coil water flow ratio to the chiller water coil
	ThermalStorage:Ice:Detailed,  ! - ice storage tank type
	Ice Tank,                !- Ice storage tank Name
	0.9,					 ! - ice fraction below which starts charging
    0.0,                   !-entering coolant temperature to chiller at 0 tank fraction [C]
    TempVsFraction;        !- offset temperature deviation to the starting temperature [K] as a function of the tank fraction 

IDD Objects (Modified):

CoilSystem:IntegratedHeatPump:AirSource,

      \memo This object is used for air-source integrated heat pump, a collection of its working modes.

    A1, \field Name
        \required-field
        \type alpha
        \reference IntegratedHeatPumps
        \note Unique name for this instance of an air-source integrated heat pump.
    A2, \field Supply Hot Water Flow Sensor Node Name
        \type node
    A3, \field Space Cooling Coil  Name
        \required-field
        \type object-list
        \object-list CoolingCoilsDXVariableSpeed
        \note Must match the name used in the corresponding Coil:Cooling:DX:VariableSpeed object.
    A4, \field Space Heating Coil Name
        \type object-list
        \object-list HeatingCoilsDXVariableSpeed
        \note Must match the name used in the corresponding Coil:Heating:DX:VariableSpeed object.
    A5, \field Dedicated Water Heating Coil Name
        \type object-list
        \object-list HeatPumpWaterHeaterDXCoilsVariableSpeed
        \note Must match the name used in the corresponding Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed object.
    A6, \field SCWH Coil Name
        \type object-list
        \object-list HeatPumpWaterHeaterDXCoilsVariableSpeed
        \note Must match the name used in the corresponding Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed object.
    A7, \field SCDWH Cooling Coil Name
        \type object-list
        \object-list CoolingCoilsDXVariableSpeed
        \note Must match the name used in the corresponding Coil:Cooling:DX:VariableSpeed object.
    A8, \field SCDWH Water Heating Coil Name
        \type object-list
        \object-list HeatPumpWaterHeaterDXCoilsVariableSpeed
        \note Must match the name used in the corresponding Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed object.
    A9, \field SHDWH Heating Coil Name
        \type object-list
        \object-list HeatingCoilsDXVariableSpeed
        \note Must match the name used in the corresponding Coil:Heating:DX:VariableSpeed object.
    A10, \field SHDWH Water Heating Coil Name
        \type object-list
        \object-list HeatPumpWaterHeaterDXCoilsVariableSpeed
        \note Must match the name used in the corresponding Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed object.
    A11, \field Enhanced Dehumidification Cooling Coil Name
        \type object-list
        \object-list CoolingCoilsDXVariableSpeed
        \note Must match the name used in the corresponding Coil:Cooling:DX:VariableSpeed object.
    A12, \field Grid Response Cooling Coil Name
        \type object-list
        \object-list CoolingCoilsDXVariableSpeed
        \note Must match the name used in the corresponding Coil:Cooling:DX:VariableSpeed object.
    A13, \field Grid Response Space Heating Coil Name
        \type object-list
        \object-list HeatingCoilsDXVariableSpeed
        \note Must match the name used in the corresponding Coil:Heating:DX:VariableSpeed object.
    N1 , \field Indoor Temperature Limit for SCWH Mode
        \type real
        \units C
        \minimum> 15.0
        \default 20.0
        \note Indoor Temperature above which Indoor Overcooling is Allowed during Cooling Operation
    N2 , \field Ambient Temperature Limit for SCWH Mode
        \type real
        \units C
        \minimum> 20.0
        \default 27.0
        \note Ambient Temperature above which Indoor Overcooling is Allowed during Cooling Operation
    N3 , \field Indoor Temperature above Which WH has Higher Priority
        \type real
        \units C
        \minimum> 15.0
        \default 20.0
        \note Indoor Temperature above which Water Heating has the higher priority and Space Heating Call Can be ignored.
    N4 , \field Ambient Temperature above Which WH has Higher Priority
        \type real
        \units C
        \minimum> 15.0
        \default 20.0
        \note Ambient Temperature above which Water Heating has the higher priority and Space Heating Call Can be ignored.
    N5 , \field Flag to Indicate Load Control in SCWH Mode
        \units dimensionless
        \type integer
        \default 0
        \note 0: match space cooling load in SCWH mode, 1: match water heating load in SCWH mode
    N6 , \field Minimum Speed Level for SCWH Mode
        \units dimensionless
        \type integer
        \default 1
        \minimum> 0
        \maximum< 10
    N7 , \field Maximum Water Flow Volume before Switching from SCDWH to SCWH Mode
        \type real
        \units m3
        \default 0.0
    N8 , \field Minimum Speed Level for SCDWH Mode
        \units dimensionless
        \type integer
        \default 1
        \minimum> 0
        \maximum< 10
    N9 , \field Maximum Running Time before Allowing Electric Resistance Heat Use during SHDWH Mode
        \type real
        \units s
        \minimum> 0.0
        \default 360.0
    N10, \field Minimum Speed Level for SHDWH Mode
        \units dimensionless
        \type integer
        \default 1
        \minimum> 0
        \maximum< 10
    N11, \field Sizing Ratio of Space Heating Coil to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 1.0
        \minimum> 0
	    \maximum< 5.0
    N12, \field Sizing Ratio of Dedicated Water Heating Coil to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 1.0
        \minimum> 0
	    \maximum< 5.0
    N13, \field Sizing Ratio of Combined Space Cooling and Water Heating Coil with Full Condensing to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 1.0
        \minimum> 0
	    \maximum< 5.0
    N14, \field Sizing Ratio of Combined Space Cooling and Water Heating Coil with Desuperheating - Cooling Capacity to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 1.0
        \minimum> 0
	    \maximum< 5.0
    N15, \field Sizing Ratio of Combined Space Cooling and Water Heating Coil with Desuperheating - Water Heating Capacity to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 0.15
        \minimum> 0
	    \maximum< 5.0
    N16, \field Sizing Ratio of Combined Space Heating and Water Heating Coil with Desuperheating - Space Heating Capacity to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 1.0
        \minimum> 0
	    \maximum< 5.0
    N17, \field Sizing Ratio of Combined Space Heating and Water Heating Coil with Desuperheating - Water Heating Capacity to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 0.1
        \minimum> 0
	    \maximum< 5.0
    N18, \field Sizing Ratio of Enhanced Dehumidification Coil to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 0.9
        \minimum> 0
	    \maximum< 5.0
    N19, \field Sizing Ratio of Grid Response Cooling Coil to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 0.9
        \minimum> 0
	    \maximum< 5.0
    N20, \field Sizing Ratio of Grid Response Heating Coil to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 0.9
        \minimum> 0
	    \maximum< 5.0
    A14, \field Chiller Coil Name
        \type object-list
        \object-list ChillerCoilsDXVariableSpeed
        \note Must match the name used in the corresponding Coil:Chiller:AirSource:VariableSpeed object.
    A15, \field Chiller Coil Belongs to a Single or Separate Unit
        \type choice
        \key Single
        \key Separate
        \default Single
    N21, \field Chiller Compressor Run Speed
        \units dimensionless
        \type integer
	    \default 1
        \minimum> 0
	    \maximum< 10
    N22, \field Sizing Ratio of Chiller Coil to Space Cooling Coil
        \units dimensionless
        \type real
	    \default 1.0
        \minimum> 0
	    \maximum< 5.0
    A16, \field Coil Object Type
        \note Enter the type of the coil affected
        \type choice
        \key Coil:Cooling:Water
        \key Coil:Cooling:Water:Detailedgeometry
    A17, \field Coil Object Name
        \note Enter the name of the coil affected
        \type object-list
        \object-list CoolingCoilName
    N23, \field Air Flow Ratio of Water Coil to the Space Cooling Coil
        \units dimensionless
        \type real
	    \default 1.0
        \minimum> 0
	    \maximum< 100.0
    N24, \field Water Flow Ratio of Water Coil to the Chiller Coil
        \units dimensionless
        \type real
	    \default 1.0
        \minimum> 0
	    \maximum< 100.0
    A18,  \field Tank Object Type
        \type choice
        \key ThermalStorage:Ice:Detailed
        \key ThermalStorage:Ice:Simple
	    \key ThermalStorage:Pcm:Simple
    A19, \field Tank Name
        \type object-list
        \object-list IceThermalStorageEquipment 
    N25, \field Ice Fraction below which charging starts
        \units dimensionless
        \type real
	    \default 0.9
        \minimum> 0
	    \maximum< 1.0
    N26,  \field Chiller Entering Temperature at 0 Tank Fraction
        \units C
        \type real
	    \default -0.5
        \minimum> -100.0
	    \maximum< 100.0
    A20; \field Temperature Deviation Curve Name, as a Function of the Tank Fraction
        \type object-list
        \object-list UnivariateFunctions
        \note quadratic curve = a + b*Fraction + c*Fraction**2
        \note cubic curve = a + b*Fraction + c*Fraction**2 + d*Fraction**3
        \note Fraction = tank ice fraction
	   