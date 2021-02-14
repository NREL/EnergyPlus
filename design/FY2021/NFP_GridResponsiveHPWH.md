# Grid Responsive Heat Pump Water Heater

**Bo Shen, ORNL**

- February 14, 2021 - Initial Draft

## Justification for Feature Update
Although the present E+ can simulate single-speed and variable-speed heat pump water heaters (HPWH), the logic responding to a grid signal to adjust the compressor speed, or on/off is still missing. This works aims to develop HVAC flexibility measures in EnergyPlus to assess impact of grid-responsive building equipment technologies and energy storage, in the aspect of variable-speed heat pump water heaters. 

## Overview
Present EnergyPlus is capable of modeling air-source variable-speed heat pump water heater. They can be grouped in a parent object of WaterHeater:HeatPump:PumpedCondenser, associated with a water tank and supplemental heating elements. The operation of the HPWH responding to thermostat sensing the tank temperature, the HPWH turns on when the water temperature below a certain value and shuts off after the temperature rises above a value. It is necessary to add a new logic to limit the speed or turn off the HPWH when the electricity price is high, i.e. having the variable speed HPWH responding to a grid singal input as the figure below:

![VSHPWH](.\HVACFlexFigures\VSHPWH.png)

The variabl-speed HPWH heating coil limits its running speed or shut off totally, when the grid signal (electricity price) falls within a range. To do this, we will modify the existing Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed. 


## Implementation

The Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed will be embeded with four new fields to take a grid signal input,

A grid signal schedule represents an electricity hourly price. For example, the figure and schedule below depicts hourly electricity prices (cents), respectively in summer (cooling season) and winter (heating season).

![GridSignal](.\HVACFlexFigures\GridSignal.png)

A max speed level should be given to limit the HPWH coil’s power input, if a grid-responsvie operation is required. When the max speed is defined a zero, the HPWH coil will shut off. A lower bound and a upper bound define the grid-responsive operation logic. When the grid signal falls between the two boundary values, the grid responsive operation will run. 

Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed Sizing

The HPWH coil sizing strategy won’t change, i.e. accepting an input of nominal capacity or given by another sizing object, e.g ThermalStorage:Heating:Pair. 

## Testing

Two new example files has been added to demonstrate the new features. The example file takes the ``HeatPumpVSAS.idf`` file and  modifies it to "ODHeatPumpVSASGridWhStorage.idf" and "IDHeatPumpVSASGridWhStorage.idf" which simulated dual-source heat pump, grid-responsive operation. 

## Documentation

Some inline code documentation has been added, and additional documentation will be added later.

## IDD Changes and Transition

Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed modifications:
 
Four new fields will be added to define a grid signal schedule, ON/OFF signal range and max speed during the grid responsive operation. For example, in the four new fields below, the HPWH coil will operate with a max speed of 5, when the grid signal from the schedule of GRIDWINTER falls between 10 and 1000. If the HPWH coil is able to meet the indoor load with a speed level below 5, the supplemental heater is off. If the heating load demands the speed level higher than 5, the HPWH will run the max speed, and remaining load will be met by the supplemental electric element. If the max speed is zero, the HPWH heating coil will shut off during the grid responsive period. 

    GRIDWINTER,				 ! - grid signal schedule
    10.0,				 	            ! - Low bound to apply grid control
    1000.0,                                                      ! - High bound to apply grid control
    5.0,                     ! - max speed when appy grid responsive control


Input example is shown below. 

Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed,

    HPWHVSCoil,              !- Name
    1,                      !- Number of Speeds {dimensionless}
    1,                      !- Nominal Speed Level {dimensionless}
    4000.0,                  !- Rated Water Heating Capacity {W}
    29.44,                   !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}
    22.22,                   !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}
    55.72,                   !- Rated Condenser Inlet Water Temperature {C}
    autosize,                  !- Rated Evaporator Air Flow Rate {m3/s} 0.2685,  
    autosize,                 !- Rated Condenser Water Flow Rate {m3/s} 0.00016, 
    No,                      !- Evaporator Fan Power Included in Rated COP
    No,                      !- Condenser Pump Power Included in Rated COP
    No,                      !- Condenser Pump Heat Included in Rated Heating Capacity and Rated COP
    0.1,                     !- Fraction of Condenser Pump Heat to Water
    HPOutdoorAirInletNode,        !- Evaporator Air Inlet Node Name
    HPWHAirFanInletNode,       !- Evaporator Air Outlet Node Name
    HPWaterInletNode,        !- Condenser Water Inlet Node Name
    HPWaterOutletNode,       !- Condenser Water Outlet Node Name
    100.0,                   !- Crankcase Heater Capacity {W}
    5.0,                     !- Maximum Ambient Temperature for Crankcase Heater Operation {C}
    WetBulbTemperature,      !- Evaporator Air Temperature Type for Curve Objects
    HPWHPLFFPLR,             !- Part Load Fraction Correlation Curve Name
	GRIDALL,				 ! - grid signal schedule
	10.0,				 	 ! - Low bound to apply grid responsive control
	1000.0,                  ! - High bound to apply grid responsive control
	5.0,                     ! - max speed when apply grid responsive control
    4000.00,                 !- Rated Water Heating Capacity at Speed 1 {W}
    3.5,                     !- Rated Water Heating COP at Speed 1 {W/W}
    0.70,                    !- Rated Sensible Heat Ratio at Speed 1
    0.201400,                !- Speed 1 Reference Unit Rated Air Flow Rate {m3/s}
    0.000179,                !- Speed 1 Reference Unit Rated Water Flow Rate {m3/s}
    10.0,                    !- Speed 1 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    HPWHHeatingCapFTemp,     !- Speed 1 Total WH Capacity Function of Temperature Curve Name
    ConstantCubic,           !- Speed 1 Total WH Capacity Function of Air Flow Fraction Curve Name
    ConstantCubic,           !- Speed 1 Total WH Capacity Function of Water Flow Fraction Curve Name
    HPWHHeatingCOPFTemp,     !- Speed 1 COP Function of Temperature Curve Name
    ConstantCubic,           !- Speed 1 COP Function of Air Flow Fraction Curve Name
    ConstantCubic,           !- Speed 1 COP Function of Water Flow Fraction Curve Name
    ......
    ,                 !- Rated Water Heating Capacity at Speed 10 {W}
    ,                     !- Rated Water Heating COP at Speed 10 {W/W}
    ,                    !- Rated Sensible Heat Ratio at Speed 10
    ,                !- Speed 10 Reference Unit Rated Air Flow Rate {m3/s}
    ,                !- Speed 10 Reference Unit Rated Water Flow Rate {m3/s}
    ,               !- Speed 10 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    ,     !- Speed 10 Total WH Capacity Function of Temperature Curve Name
    ,           !- Speed 10 Total WH Capacity Function of Air Flow Fraction Curve Name
    ,           !- Speed 10 Total WH Capacity Function of Water Flow Fraction Curve Name
    ,     !- Speed 10 COP Function of Temperature Curve Name
    ,           !- Speed 10 COP Function of Air Flow Fraction Curve Name
    ;           !- Speed 10 COP Function of Water Flow Fraction Curve Name



IDD Objects (Modified):
Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed 

The new fields are highlighted in red (A11, N12, N13, N14) 

Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed,

        \memo vairlable-speed Heat pump water heater (VSHPWH) heating coil, air-to-water direct-expansion (DX)
        \memo system which includes a variable-speed water heating coil, evaporator air coil, evaporator
        \memo fan, electric compressor, and water pump. Part of a WaterHeater:HeatPump system.
        \min-fields 33
    A1 , \field Name
        \required-field
        \type alpha
        \reference HeatPumpWaterHeaterDXCoilsVariableSpeed
        \note Unique name for this instance of a variable-speed heat pump water heater DX coil.
    N1,  \field Number of Speeds
        \units dimensionless
        \type integer
        \minimum 1
        \maximum 10
        \default 1
    N2 , \field Nominal Speed Level
        \units dimensionless
        \type integer
        \default 1
        \note must be lower than or equal to the highest speed number
    N3 , \field Rated Water Heating Capacity
        \required-field
        \type real
        \units W
        \minimum> 0
        \note Water Heating capacity at the rated inlet air temperatures, rated condenser inlet
        \note water temperature, rated air flow rate, and rated water flow rate.
        \note Can optionally include condenser pump heat.
    N4 , \field Rated Evaporator Inlet Air Dry-Bulb Temperature
        \type real
        \units C
        \minimum> 5
        \default 19.7
        \note Evaporator inlet air dry-bulb temperature corresponding to rated coil performance
        \note (heating capacity, COP and SHR).
    N5 , \field Rated Evaporator Inlet Air Wet-Bulb Temperature
        \type real
        \units C
        \minimum> 5
        \default 13.5
        \note Evaporator inlet air wet-bulb temperature corresponding to rated coil performance
        \note (heating capacity, COP and SHR).
    N6 , \field Rated Condenser Inlet Water Temperature
        \type real
        \units C
        \minimum> 25
        \default 57.5
        \note Condenser inlet water temperature corresponding to rated coil performance
        \note (heating capacity, COP and SHR).
    N7 , \field Rated Evaporator Air Flow Rate
        \type real
        \units m3/s
        \minimum> 0
        \autocalculatable
        \note Evaporator air flow rate corresponding to rated coil performance
        \note (heating capacity, COP and SHR).
        \note Default is 5.035E-5 m3/s/W (31.25 cfm/MBH) of rated heating capacity when autocalculated.
    N8 , \field Rated Condenser Water Flow Rate
        \type real
        \units m3/s
        \ip-units gal/min
        \minimum> 0
        \autocalculatable
        \note Condenser water flow rate corresponding to rated coil performance
        \note (heating capacity, COP and SHR).
        \note Default is 4.487E-8 m3/s/W (0.208 gpm/MBH) of rated heating capacity when autocalculated.
        \note A warning message will be issued if the ratio of Rated Condenser Water Flow Rate
        \note to Heating Capacity is less than 1.79405E-8 m3/s/W (0.083 gpm/MBH)
        \note or greater than 8.97024E-8 m3/s/W (0.417 gpm/MBH), but the simulation will continue.
    A2 , \field Evaporator Fan Power Included in Rated COP
        \type choice
        \key Yes
        \key No
        \default Yes
        \note Select Yes if the evaporator fan power is included in the rated COP. This choice field
        \note impacts the calculation of compressor electric power.
    A3 , \field Condenser Pump Power Included in Rated COP
        \type choice
        \key Yes
        \key No
        \default No
        \note Select Yes if the condenser pump power is included in the rated COP. This choice field
        \note impacts the calculation of compressor electric power.
    A4 , \field Condenser Pump Heat Included in Rated Heating Capacity and Rated COP
        \type choice
        \key Yes
        \key No
        \default No
        \note Select Yes if the condenser pump heat is included in the rated heating capacity and
        \note rated COP. This choice field impacts the calculation of water heating capacity.
    N9 , \field Fraction of Condenser Pump Heat to Water
        \type real
        \minimum 0
        \maximum 1
        \default 0.2
        \note Fraction of pump heat transferred to the condenser water. The pump is assumed
        \note to be located downstream of the condenser.
    A5 , \field Evaporator Air Inlet Node Name
        \required-field
        \type node
        \note The node from which the DX coil draws its inlet air.
    A6 , \field Evaporator Air Outlet Node Name
        \required-field
        \type node
        \note The node to which the DX coil sends its outlet air.
    A7 , \field Condenser Water Inlet Node Name
        \required-field
        \type node
        \note The node from which the DX coil condenser draws its inlet water.
        \note This name should match the source side outlet node name in the associated
        \note water heater tank object.
    A8 , \field Condenser Water Outlet Node Name
        \required-field
        \type node
        \note The node to which the DX coil condenser sends its outlet water.
        \note This name should match the source side inlet node name in the associated
        \note water heater tank object.
    N10, \field Crankcase Heater Capacity
        \type real
        \minimum 0
        \default 0
        \units W
        \note The compressor crankcase heater only operates when the dry-bulb temperature of air
        \note surrounding the compressor is below the Maximum Ambient Temperature for Crankcase
        \note Heater Operation and the DX coil is off.  The ambient temperature surrounding the
        \note compressor is set by the WaterHeater:HeatPump parent object (field Compressor Location).
    N11, \field Maximum Ambient Temperature for Crankcase Heater Operation
        \type real
        \minimum 0
        \default 10
        \units C
        \note The compressor crankcase heater only operates when the dry-bulb temperature of air
        \note surrounding the compressor is below the Maximum Outdoor Temperature for Crankcase
        \note Heater Operation and the unit is off. The ambient temperature surrounding the
        \note compressor is set by the WaterHeater:HeatPump parent object (field Compressor Location).
    A9 , \field Evaporator Air Temperature Type for Curve Objects
        \type choice
        \key DryBulbTemperature
        \key WetBulbTemperature
        \default WetBulbTemperature
        \note Determines temperature type for heating capacity curves and
        \note heating COP curves. This input determines whether
        \note the inlet air dry-bulb or wet-bulb temperature is used to evaluate these curves.
    A10, \field Part Load Fraction Correlation Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note Part Load Fraction Correlation (function of part load ratio) should be quadratic or cubic.
        \note Quadratic curve = a + b(PLR) + c(PLR)^2.
        \note Cubic curve = a + b(PLR) + c(PLR)^2 + d(PLR)^3.
        \note PLR = part load ratio (heating delivered/steady state heating capacity).
        \note Use curve coefficients of 1,0,0 or leave this field blank when neglecting performance impacts
        \note due to variations in part load ratio.
    A11, \field Grid Signal Schedule Name
        \type object-list
        \object-list ScheduleNames
        \note This field refers to a grid signal pattern, e.g. price 
    N12, \field Lower Bound to Apply Grid Responsive Control
        \type real
        \units dimensionless
		\default 100.0
    N13, \field Upper Bound to Apply Grid Responsive Control
        \type real
        \units dimensionless
		\default -100.0
    N14, \field Max Speed Level During Grid-Responsive Control
        \type real
        \units dimensionless   
		\default 10.0
    N15, \field Rated Water Heating Capacity at Speed 1
        \required-field
        \type real
        \units W
        \minimum> 0
        \note Heating capacity at the rated inlet air temperatures, rated condenser inlet
        \note water temperature, rated air flow rate, and rated water flow rate.
        \note Can optionally include condenser pump heat.
    ......
    A71; \field Speed 10 COP Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow

		
New Fields:  

    Field: Grid Signal Schedule Name
    This alpha field refers to grid signal schedule name. 
    Field: Lower Bound to Apply Grid Responsive Control
    This numeric field contains a lower bound to apply the grid responsive control, for example, if electricity price obtained from a Grid signal schedule is above 10.0 (cents), the grid responsive control will be applied. If there is no lower bound, one can give a large negative number, e.g. -10000.0. 
    Field: Uppler Bound to Apply Grid Responsive Control
    This numeric field defines contains an upper bound to apply a grid responsive control. If there is no upper bound, one can give a very large positive number, e.g. 10000.0.
    Field: Max Speed Level During Grid-Responsive Control
    This numeric field contains the max allowed speed when the grid responsive control is ON. If the heating load demands the speed level higher than the max speed, the heating coil runs the max speed, and remaining load will be matched by a supplemental heater. If the max speed is zero, the HPWH coil shuts off during the grid responsive period. 
