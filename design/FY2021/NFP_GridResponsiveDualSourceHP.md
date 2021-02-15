# Grid Responsive Dual Source Heat Pump

**Bo Shen and Jian Sun, ORNL**

- February 14, 2021 - Initial Draft

## Justification for Feature Update
Although the present E+ can simulate multiple heating coils, i.e. variable-speed direct expansion (DX) heating coil, gas heating coil and hot water heating coil, these models have not be integrated to simulate a dual-source heat pump, i.e. electricity-driven and fuel-driven heating, in response to grid signals. This work aims to Develop HVAC flexibility measures in EnergyPlus to assess impact of grid-responsive building equipment technologies and energy storage, in the aspect of dual-source heat pumps. 

## Overview
Present EnergyPlus is capable of modeling variable-speed DX heating coils, gas heating and water heating coils. They can be grouped in a parent object of AirLoopHVAC:UnitaryHeatPump, given in the figure below. The Coil:Heating:Fuel and Coil:Heating:Water serves as the supplemental heating coil to compensate the heating capacity when the main Coil:Heating:DX:VariableSpeed coil fails to meet the indoor load. 

![DualHP](.\HVACFlexFigures\DualHP.png)

The operation of the two heating coils basically reponds to a single unit ON/OFF schedule. It is necessary to add a new logic to limit the speed or turn off the DX heating coil when the electricity price is high, i.e. having the variable speed DX coil responding to a grid singal input as the figure below:

![GridDualHP](.\HVACFlexFigures\GridDualHP.png)

The variabl-speed DX heating coil limits its running speed or shut off totally, when the grid signal (electricity price) falls within a range. In this way, the supplemental heating coil will automatically compensate the capacity and achieve a grid responsive dual-source operation. To do this, we will modify the existing Coil:Heating:DX:VariableSpeed

## Implementation

The Coil:Heating:DX:VariableSpeed will be embeded with four new fields to take a grid signal input. The new codes were added to VariableSpeedCoils.cc and VariableSpeedCoils.hh.

A grid signal schedule represents an electricity hourly price. For example, the figure and schedule below depicts hourly electricity prices (cents), respectively in summer (cooling season) and winter (heating season).
![GridSignal](.\HVACFlexFigures\GridSignal.png)

A max speed level should be given to limit the heating coil’s power input, if a grid-responsvie operation is required. When the max speed is defined a zero, the heating coil will shut off. A lower bound and a upper bound define the grid-responsive operation logic. When the grid signal fails between the two boundary values, the grid responsive operation will run. 

Coil:Heating:DX:VariableSpeed Sizing
The DX coil sizing strategy won’t change, which size the heating coil to meet the zonal peak heating load assuming no grid-responsive request. 


## Testing

A new example file has been added to demonstrate the new features. The example file takes the ``HeatPumpVSAS.idf`` file and  modifies it to "HeatPumpVSASGrid.idf" which simulated dual-source heat pump, grid-responsive operation. 
## Documentation

Some inline code documentation has been added, and additional documentation will be added later.

## IDD Changes and Transition

Coil:Heating:DX:VariableSpeed modifications:
 
Four new fields will be added to define a grid signal schedule, ON/OFF signal range and max speed during the grid responsive operation. For example, in the four new fields below, the heating coil will operate with a max speed of 5, when the grid signal from the schedule of GRIDWINTER falls between 10 and 1000. If the heating coil can meet the indoor load with a speed level below 5, the supplemental coil is off. If the heating load demands the speed level higher than 5, the heating coil will run the max speed, and remaining load will be met by the supplemental heating coil, e.g. gas or hot water. If the max speed is zero, the DX heating coil will shut off during the grid responsive period. 

    GRIDWINTER,				 ! - grid signal schedule
    10.0,				 	            ! - Low bound to apply grid control
    1000.0,                                                      ! - High bound to apply grid control
    5.0,                     ! - max speed when appy grid responsive control


Input example is shown below. 

Coil:Heating:DX:VariableSpeed,

    Sys 1 Heat Pump Heating Mode,  				!- Name
    Sys 1 Heating Coil Air Inlet Node,  				!- Air Inlet Node Name
    Sys 1 SuppHeating Coil Air Inlet Node,  			!- Air Outlet Node Name
    10.0,								!- Number of Speeds
    10.0,								!- Nominal Speed Level
    Autosize,                					!- Rated Heating Capacity {W}
    Autosize,                     					!- Rated Air Flow Rate {m3/s}
    VS Energy Part Load Fraction 1,    				!- Energy part load fraction curve
    VS Defrost Power Function 1,            !Defrost Energy Input Ratio Function of Temperature Curve Name
    0,                                      !Minimum Outdoor DB Temperature for Compressor Operation {C}
    47,                                     !Maximum Outdoor DB Temperature for Defrost Operation {C}
    200,                                    !Crankcase Heater Capacity {W}
    15,                                !Maximum Outdoor DB Temperature for Crankcase Heater Operation {C}
    Reverse Cycle,                     !Defrost Strategy
    OnDemand,                          !Defrost Control
    0.0,                               !Defrost Time Period Fraction
    0.0,                               !Resistive Defrost Heater Capacity (W)
    GRIDWINTER,			 ! - grid signal schedule 
    10.0,				 	 ! - Low bound to apply grid responsive control
    1000.0,                  ! - High bound to apply grid responsive control
    5.0,                     ! - max speed when appy grid responsive control   
    1838.7,		             			           !- Speed 1 Reference Unit Heating Capacity
    5.0,                             				!- Speed 1 Reference Unit COP
    0.1661088,                             			!- Speed 1 Reference Unit Air Flow Rate
    Heating VS Temp1 Test,    !- Speed 1 Total Heating Capacity Function of Temperature Curve Name
    Heating VS AirFrac Test,  !- Speed 1 Total Heating Capacity Function of Air Flow Fraction Curve Name
    EIRH VS Temp1 Test,       !- Speed 1 Energy Input Ratio Function of Temperature Curve Name
    EIRH VS AirFrac Test,     !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name
    ....
    EIRH VS AirFrac Test; 	!- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name
    

IDD Objects (Modified):
Coil:Heating:DX:VariableSpeed
The new fields are given at A8, N12, N13, N14. 

Coil:Heating:DX:VariableSpeed,

    \memo Direct expansion (DX) heating coil (air-to-air heat pump) and compressor unit
    \memo (includes electric compressor and outdoor fan), variable-speed, with defrost
    \memo controls. Requires two to ten sets of performance data and will interpolate between
    \memo speeds.
    \min-fields 25
    A1,  \field Name
    \required-field
    \type alpha
    \reference HeatingCoilsDXVariableSpeed
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
    N3,  \field Rated Heating Capacity At Selected Nominal Speed Level
        \units W
        \type real
        \autosizable
        \default autosize
    N4,  \field Rated Air Flow Rate At Selected Nominal Speed Level
        \units m3/s
        \type real
        \autosizable
        \default autosize
    A4,  \field Energy Part Load Fraction Curve Name
        \required-field
        \type object-list
        \object-list UnivariateFunctions
        \note quadratic curve = a + b*PLR + c*PLR**2
        \note cubic curve = a + b*PLR + c*PLR**2 + d*PLR**3
        \note PLR = part load ratio (heating load/steady state capacity)
    A5,  \field Defrost Energy Input Ratio Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note biquadratic curve = a + b*wb + c*wb**2 + d*oat + e*oat**2 + f*wb*oat
        \note wb = wet-bulb temperature (C) of air entering the indoor coil
        \note oat = outdoor air dry-bulb temperature (C)
        \note only required if ReverseCycle defrost strategy is specified
    N5,  \field Minimum Outdoor Dry-Bulb Temperature for Compressor Operation
        \type real
        \default -8.0
        \units C
    N6,  \field Outdoor Dry-Bulb Temperature to Turn On Compressor
        \type real
        \units C
        \note The outdoor temperature when the compressor is automatically turned back on following an
        \note automatic shut off because of low outdoor dry-bulb temperature. This field is only used
        \note for the calculation of HSPF. If this field is not provided, then outdoor bin temperature
        \note used in the HSPF calculation is always considered to be greater than this temperature and
        \note 'Minimum Outdoor Dry-Bulb Temperature for Compressor Operation' field described above.
        \note This assumption is based on AHRI standard 210/240 (2008) and can introduce significant error
        \note in the final value of HSPF.
    N7,  \field Maximum Outdoor Dry-Bulb Temperature for Defrost Operation
        \type real
        \minimum 0.0
        \maximum 7.22
        \default 5.0
        \units C
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
    A6,  \field Defrost Strategy
        \type choice
        \key ReverseCycle
        \key Resistive
        \default ReverseCycle
    A7,  \field Defrost Control
        \type choice
        \key Timed
        \key OnDemand
        \default Timed
    N10, \field Defrost Time Period Fraction
        \type real
        \minimum 0.0
        \default 0.058333
        \note Fraction of time in defrost mode
        \note only applicable if timed defrost control is specified
    N11, \field Resistive Defrost Heater Capacity
        \type real
        \minimum 0.0
        \default 0.0
        \autosizable
        \units W
        \note only applicable if resistive defrost strategy is specified
        \ip-units W
    A8, \field Grid Signal Schedule Name
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
    N15, \field Speed 1 Reference Unit Gross Rated Heating Capacity
        \note Heating capacity not accounting for the effect of supply air fan heat
        \units W
        \type real
        \minimum 0
        \required-field
    N16, \field Speed 1 Reference Unit Gross Rated Heating COP
        \type real
        \units W/W
        \minimum> 0.0
        \required-field
    N17,  \field Speed 1 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
        \required-field
    A9,  \field Speed 1 Heating Capacity Function of Temperature Curve Name
        \required-field
        \type object-list
        \object-list BivariateFunctions
        \note curve = a + b*db + c*db**2 + d*oat + e*oat**2 + f*db*oat
        \note db = entering air dry-bulb temperature (C)
        \note oat = air entering temperature seen by the evaporator (C)
    A10,  \field Speed 1 Total  Heating Capacity Function of Air Flow Fraction Curve Name
        \required-field
        \type object-list
        \object-list UnivariateFunctions
        \note quadratic curve = a + b*ffa + c*ffa**2
        \note cubic curve = a + b*ffa + c*ffa**2 + d*ffa**3
        \note ffa = Fraction of the full load Air Flow
    A11, \field Speed 1 Energy Input Ratio Function of Temperature Curve Name
        \required-field
        \type object-list
        \object-list BivariateFunctions
        \note curve = a + b*db + c*db**2 + d*oat + e*oat**2 + f*db*oat
        \note db = entering air dry-bulb temperature (C)
        \note oat = air entering temperature seen by the evaporator (C)
    A12, \field Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name
        \required-field
        \type object-list
        \object-list UnivariateFunctions
        \note quadratic curve = a + b*ffa + c*ffa**2
        \note cubic curve = a + b*ffa + c*ffa**2 + d*ffa**3
        \note ffa = Fraction of the full load Air Flow
    ......
    A48; \field Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note quadratic curve = a + b*ffa + c*ffa**2
        \note cubic curve = a + b*ffa + c*ffa**2 + d*ffa**3
        \note ffa = Fraction of the full load Air Flow  
		
New Fields:  

    Field: Grid Signal Schedule Name - 
    This alpha field refers to grid signal schedule name. 
    Field: Lower Bound to Apply Grid Responsive Control   
    This numeric field contains a lower bound to apply the grid responsive control, for example, if electricity price obtained from a Grid signal schedule is above 10.0 (cents), the grid responsive control will be applied. If there is no lower bound, one can give a very small negative number, e.g. -10000.0. 
    Field: Uppler Bound to Apply Grid Responsive Control    
    This numeric field defines contains an upper bound to apply a grid responsive control. If there is no upper bound, one can give a very large positive number, e.g. 10000.0.
    Field: Max Speed Level During Grid-Responsive Control    
    This numeric field contains the max allowed speed when the grid responsive control is ON. If the heating load demands the speed level higher than the max speed, the heating coil runs the max speed, and remaining load will be matched by the supplemental heating coil, e.g. gas or hot water. If the max speed is zero, the DX heating coil shuts off during the grid responsive period. 

