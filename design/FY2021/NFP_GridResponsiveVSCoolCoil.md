# Grid Responsive Variable-Speed Cooling Coil for Separate Sensible and Latent Cooling

**Bo Shen and Jian Sun, ORNL**

- February 14, 2021 - Initial Draft

## Justification for Feature Update
To Develop HVAC flexibility measures in EnergyPlus and assess impact of grid-responsive building equipment technologies and energy storage, there is a need to operate the variable-speed cooling coil during grid peak load hours at a reduced power input.That will limit the top cooling capacity below the indoor cooling demand. Because the reduced cooling capacity can’t meet the sensible and latent load simulatenously, one may choose to control the sensible or latent load. It is necessary to incorporate the control logic in the existing variable-speed DX (direct expansion) cooling coil.  

## Overview
Present EnergyPlus is capable of modeling variable-speed DX cooling coil. It can be embededed in a parent object of AirLoopHVAC:UnitarySystem, given in the figure below. 

![VSCoolingCoil](.\HVACFlexFigures\VSCoolingCoil.png)

One control schedule turns on/off the cooling coil, and can do separate sensible and latent cooling. However, the logic to limit the cooling power input during grid peak hours is missing. It is necessary to add a new logic to limit the top speed or turn off the DX cooling coil when the electricity price is high, i.e. having the variable speed DX coil responding to a grid singal input as the figure below:

![GridVSCoolCoil](.\HVACFlexFigures\GridVSCoolCoil.png)

The variabl-speed DX cooling coil limits its running speed or shut off totally, when the grid signal (electricity price) falls within a range. Additionally, the variable speed coil operates to manage sensible or latent load selectively, where a related ZoneControl:Humidistat object is also required. To do this, we will modify the existing Coil:Cooling:DX:VariableSpeed.

## Implementation

The Coil:Cooling:DX:VariableSpeed will be embeded with five new fields to take a grid signal input and load control request. The new codes were added to VariableSpeedCoils.cc and VariableSpeedCoils.hh.

A grid signal schedule represents an electricity hourly price. For example, the figure and schedule below depicts hourly electricity prices (cents), respectively in summer (cooling season) and winter (heating season).

![GridSignal](.\HVACFlexFigures\GridSignal.png)

A max speed level should be given to limit the cooling coil’s power input, when a grid-responsvie operation is required. When the max speed is defined a zero, the cooling coil will shut off. A lower bound and a upper bound define the grid-responsive operation logic. When the grid signal falls between the two boundary values, the grid responsive operation will run. Another flag will define the sensible or latent load control. 

Coil:Cooling:DX:VariableSpeed Sizing: The DX coil sizing strategy won’t change, which sizes the cooling coil to meet the zonal peak sensible load assuming no grid-responsive request. 

## Testing

A new example file has been added to demonstrate the new features. The example file takes the ``HeatPumpVSAS.idf`` file and  modifies it to "HeatPumpVSASGridWh.idf" which simulated grid-responsive cooling coil, having separate sensible and latent cooling control.

## Documentation

Some inline code documentation has been added, and additional documentation will be added later.

## IDD Changes and Transition

Coil:Cooling:DX:VariableSpeed modifications:

Five new fields will be added to define a grid signal schedule, ON/OFF signal range and max speed during the grid responsive operation. For example, in the five new fields below, the cooling coil will operate with a max speed of 5, when the grid signal from the schedule of GRIDSUMMER falls between 10 and 1000. If the cooling load demands the speed level higher than 5, the cooling coil will run the max speed. If the max speed is zero, the DX heating coil will shut off during the grid responsive period. The load matching input defines the mode to meet the sensible load only (key = “Sensible”), or latent load only (key = “Latent”). If this field is empty or key = “SenLat”, the sensible or latent load control will be determined in the parent object, i.e AirLoopHVAC:UnitarySystem or AirLoopHVAC:UnitaryHeatPump. 

    GRIDSUMMER,				 ! - grid signal schedule
    10.0,				     ! - Low bound to apply grid responsive control
    1000.0,                  ! - High bound to apply grid responsive control
    5.0,                     ! - max speed when appy grid responsive control
    Sensible,				 ! - load matched during grid responsive operation




Input example is shown below. 

Coil:Cooling:DX:VariableSpeed,

    Heat Pump ACDXCoil 1,    !- Name
    DX Cooling Coil Air Inlet Node,  !- Indoor Air Inlet Node Name
    Heating Coil Air Inlet Node,  !- Indoor Air Outlet Node Name
    10.0,                    !- Number of Speeds {dimensionless}
    10.0,                    !- Nominal Speed Level {dimensionless}
    32000,                   !- Gross Rated Total Cooling Capacity At Selected Nominal Speed Level {W}
    1.7,                     !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}
    0.0,                     !- Nominal Time for Condensate to Begin Leaving the Coil {s}
    0.0,                     !- Initial Moisture Evaporation Rate Divided by Steady-State AC Latent Capacity {dimensionless}
    HPACCOOLPLFFPLR,         !- Energy Part Load Fraction Curve Name
    ,                        !- Condenser Air Inlet Node Name
    AirCooled,               !- Condenser Type
    ,                        !- Evaporative Condenser Pump Rated Power Consumption {W}
    200.0,                   !- Crankcase Heater Capacity {W}
    10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}
    ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}
    ,                        !- Supply Water Storage Tank Name
    ,                        !- Condensate Collection Water Storage Tank Name
    ,                        !- Basin Heater Capacity {W/K}
    ,                        !- Basin Heater Setpoint Temperature {C}
    ,                        !- Basin Heater Operating Schedule Name
	GRIDSUMMER,				 ! - grid signal schedule
	10.0,				     ! - Low bound to apply grid responsive control
	1000.0,                  ! - High bound to apply grid responsive control
	5.0,                     ! - max speed when appy grid responsive control
	Sensible,				 ! - load matched during grid responsive operation
    1524.1,                  !- Speed 1 Reference Unit Gross Rated Total Cooling Capacity {W}
    0.75,                    !- Speed 1 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}
    4.0,                     !- Speed 1 Reference Unit Gross Rated Cooling COP {dimensionless}
    0.1359072,               !- Speed 1 Reference Unit Rated Air Flow Rate {m3/s}
    0.26,                    !- Speed 1 Reference Unit Rated Condenser Air Flow Rate {m3/s}
    ,                        !- Speed 1 Reference Unit Rated Pad Effectiveness of Evap Precooling 
    HPACCoolCapFT,           !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name
    HPACCoolCapFFF,          !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name
    HPACCOOLEIRFT,           !- Speed 1 Energy Input Ratio Function of Temperature Curve Name
    HPACCOOLEIRFFF,          !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name
    ......
    6758.0,                  !- Speed 10 Reference Unit Gross Rated Total Cooling Capacity {W}
    0.75,                    !- Speed 10 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}
    4.0,                     !- Speed 10 Reference Unit Gross Rated Cooling COP {dimensionless}
    0.37752,                 !- Speed 10 Reference Unit Rated Air Flow Rate {m3/s}
    0.74,                    !- Speed 10 Reference Unit Condenser Air Flow Rate {m3/s}
    ,                        !- Speed 10 Reference Unit Rated Pad Effectiveness of Evap Precooling 
    HPACCoolCapFT,           !- Speed 10 Total Cooling Capacity Function of Temperature Curve Name
    HPACCoolCapFFF,          !- Speed 10 Total Cooling Capacity Function of Air Flow Fraction Curve Name
    HPACCOOLEIRFT,           !- Speed 10 Energy Input Ratio Function of Temperature Curve Name
    HPACCOOLEIRFFF;          !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name

    

IDD Objects (Modified): 

Coil:Cooling:DX:VariableSpeed

The new fields are given at A10, N13, N14, N15, A11. 

Coil:Cooling:DX:VariableSpeed,

    \memo Direct expansion (DX) cooling coil and condensing unit (includes electric compressor
    \memo and condenser fan), variable-speed. Optional inputs for moisture evaporation from
    \memo wet coil when compressor cycles off with continuous fan operation. Requires two to
    \memo ten sets of performance data and will interpolate between speeds. Modeled as a
    \memo single coil with variable-speed compressor.
    \min-fields 31
    A1,  \field Name
        \required-field
        \type alpha
        \reference CoolingCoilsDXVariableSpeed
        \reference DesuperHeatingCoilSources
    A2,  \field Indoor Air Inlet Node Name
        \required-field
        \type node
    A3,  \field Indoor Air Outlet Node Name
        \required-field
        \type node
    N1,  \field Number of Speeds
        \units dimensionless
        \type integer
        \minimum 1
        \maximum 10
        \default 2
    N2,  \field Nominal Speed Level
        \units dimensionless
        \type integer
        \default 2
        \note must be lower than or equal to the highest speed number
    N3,  \field Gross Rated Total Cooling Capacity At Selected Nominal Speed Level
        \note Total cooling capacity not accounting for the effect of supply air fan heat
        \units W
        \type real
        \autosizable
        \default autosize
    N4,  \field Rated Air Flow Rate At Selected Nominal Speed Level
        \units m3/s
        \type real
        \autosizable
        \default autosize
    N5,  \field Nominal Time for Condensate to Begin Leaving the Coil
        \units s
        \type real
        \minimum 0
        \default 0
    N6,  \field Initial Moisture Evaporation Rate Divided by Steady-State AC Latent Capacity
        \units dimensionless
        \type real
        \minimum 0
        \default 0
    A4,  \field Energy Part Load Fraction Curve Name
        \required-field
        \type object-list
        \object-list UnivariateFunctions
        \note quadratic curve = a + b*PLR + c*PLR**2
        \note cubic curve = a + b*PLR + c*PLR**2 + d*PLR**3
        \note PLR = part load ratio (cooling load/steady state capacity)
    A5,  \field Condenser Air Inlet Node Name
        \type node
        \note Enter the name of an outdoor air node. This node name is also specified in
        \note an OutdoorAir:Node or OutdoorAir:NodeList object.
    A6,  \field Condenser Type
        \type choice
        \key AirCooled
        \key EvaporativelyCooled
        \default AirCooled
    N7,  \field Evaporative Condenser Pump Rated Power Consumption
        \type real
        \units W
        \minimum 0.0
        \default 0.0
        \autosizable
        \note Rated power consumed by the evaporative condenser's water pump
    N8,  \field Crankcase Heater Capacity
        \type real
        \minimum 0.0
        \default 0.0
        \units W
        \ip-units W
    N9,  \field Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation
        \type real
        \minimum 0.0
        \default 10.0
        \units C
    N10, \field Minimum Outdoor Dry-Bulb Temperature for Compressor Operation
        \type real
        \default -25.0
        \units C
    A7,  \field Supply Water Storage Tank Name
        \type object-list
        \object-list WaterStorageTankNames
    A8,  \field Condensate Collection Water Storage Tank Name
        \type object-list
        \object-list WaterStorageTankNames
    N11, \field Basin Heater Capacity
        \type real
        \units W/K
        \minimum 0.0
        \default 0.0
        \note This field is only used for Condenser Type = EvaporativelyCooled and for periods
        \note when the basin heater is available (field Basin Heater Operating Schedule Name).
        \note For this situation, the heater maintains the basin water temperature at the basin heater
        \note setpoint temperature when the outdoor air temperature falls below the setpoint temperature.
        \note The basin heater only operates when the DX coil is off.
    N12, \field Basin Heater Setpoint Temperature
        \type real
        \units C
        \minimum 2.0
        \default 2.0
        \note This field is only used for Condenser Type = EvaporativelyCooled.
        \note Enter the outdoor dry-bulb temperature when the basin heater turns on.
    A9,  \field Basin Heater Operating Schedule Name
        \type object-list
        \object-list ScheduleNames
        \note This field is only used for Condenser Type = EvaporativelyCooled.
        \note Schedule values greater than 0 allow the basin heater to operate whenever the outdoor
        \note air dry-bulb temperature is below the basin heater setpoint temperature.
        \note If a schedule name is not entered, the basin heater is allowed to operate
        \note throughout the entire simulation.
    A10, \field Grid Signal Schedule Name
        \type object-list
        \object-list ScheduleNames
        \note This field refers to a grid signal pattern, e.g. price 
    N13, \field Lower Bound to Apply Grid Responsive Control
        \type real
        \units dimensionless
		\default 100.0
    N14, \field Upper Bound to Apply Grid Responsive Control
        \type real
        \units dimensionless
		\default -100.0
    N15, \field Max Speed Level During Grid-Responsive Control
        \type real
        \units dimensionless   
		\default 10
    A11, \field Load Control during Grid Responsive Operation
        \type choice
        \key Sensible
        \key Latent
		\key SenLat
        \default SenLat
    N16, \field Speed 1 Reference Unit Gross Rated Total Cooling Capacity
        \note Total cooling capacity not accounting for the effect of supply air fan heat
        \units W
        \type real
        \minimum 0
        \required-field
    ...
    A51; \field Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note quadratic curve = a + b*ffa + c*ffa**2
        \note cubic curve = a + b*ffa + c*ffa**2 + d*ffa**3
        \note ffa = Fraction of the full load Air Flow 
		
New Fields:  

    Field: Grid Signal Schedule Name
    This alpha field refers to grid signal schedule name. 
    Field: Lower Bound to Apply Grid Responsive Control
    This numeric field contains a lower bound to apply the grid responsive control, for example, if electricity price obtained from a Grid signal schedule is above 10.0 (cents), the grid responsive control will be applied. If there is no lower bound, one can give a very small negative number, e.g. -10000.0. 
    Field: Uppler Bound to Apply Grid Responsive Control
    This numeric field defines contains an upper bound to apply a grid responsive control. If there is no upper bound, one can give a very large positive number, e.g. 10000.0.
    Field: Max Speed Level During Grid-Responsive Control
    This numeric field contains the max allowed speed when the grid responsive control is ON. If the cooling load demands a speed level higher than the max speed, the cooling coil runs the max speed. If the max speed is zero, the DX cooling coil shuts off during the grid responsive period. 
    Field: Load Control during Grid Responsive Operation
    This alpha field defines the load matching mode, corresponding to a ZoneControl:Humidistat object referred in the same unitary object.  The load matching input defines the mode to meet the sensible load only (key = “Sensible”), latent load only (key = “Latent”). If this field is empty or key = “SenLat”, the sensible or latent load control will be determined by the parent object, i.e AirLoopHVAC:UnitarySystem or AirLoopHVAC:UnitaryHeatPump. 

