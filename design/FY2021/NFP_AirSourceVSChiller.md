# Variable-Speed Air-Source Chiller
**Bo Shen, ORNL**

- February 14, 2021 - Initial Draft

## Justification for Feature Update
The present E+ is able to simulate single-speed air source chillers. However, it is not truly representative of more advanced variable-speed fan/pump, variable compressor air source chillers, due to using only one set of performance curves and control strategy. E+ has comprised multiple water source and air-source,variable-speed cooling and heating coils and heat pump water heaters, e.g Coil:Cooling:DX:VariableSpeed, Coil:Heating:DX:VariableSpeed, and Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed, etc. They are based on a similar methodology, to use multiple sets of performance curves at individual speed levels to represent a manfacturer’s product data. A variable-speed chiller model will comprise performance curves up to 10 speed levels. E+ iterates the speed and interpolates between speed levels to match the building cooling or heating load. We can extend this approach to model a new air-source variable-speed chiller. This development is necessary to simulate an ice making chiller, in an air loop of an integrated heat pump. This will facilitates grid-responsive ice making, and hydronic cooling, controlled by an integrated heat pump object. 

## Overview
The latest technology for air-source chillers can utilize a variable speed compressor with a variable speed pump and outdoor fan. A variable-speed chiller model will comprise performance curves up to 10 speed levels. It iterates the speed and interpolates between speed levels to match the building cooling load. The variable-speed chiller model is universal, able to simulate a single-speed chiller, by only implementing performance curves at one speed level. Integrated heat pumps are multi-functional units, capable of space conditioning and water heating. We will enhance the current E+ integrated heat pump model to simulate the operation grid-resposive ice making and hydronic cooling in an air loop. The variable-speed chiller will be referred in the integrated heat pump to provide ice making. 

In an air-source, variable-speed chiller, the water/glycol flow rate entering the evaporator coil is usually a function of the compressor speed. Refrigerant mass flow rate is a function of compressor speed as well as the water/glycol temperature entering the evaporator, and the air dry bulb temperature entering the condenser coil. The compressor, fan and pump speeds are not discrete values and can be considered to vary infinitesimally between the minimum and maximum compressor speeds. At the minimum compressor speed, for a continuous fan/pump, the supply water and outdoor air flow rates are fixed and the unit will have to cycle for reduced part loads below this point. For a cycling fan, the fan/pump will cycle with the compressor. 

To simulate the variable-speed air-source chiller, the number of speed levels and the corresponding curve sets will be expanded up to ten. The number of speed levels is selectable by the user. The user can provide speed levels at any number from 1 to 10. In the case that the number of given speed levels are above 1, the model would do linear interpolation between neighboring speeds. The more curves, the more accurate. Furthermore, using linear interpolation, and inputting air and water flow rates at individual speed levels facilitates arbitrary relationships of flow rates as a function of the compressor speed. 

The Coil:Chiller:AirSource:VariableSpeed object will simulate the performance of a variable-speed air source chiller. It fits into the parent object of CoilSystem:IntegratedHeatPump:AirSource, to be connected other components, i.e. storage of ice or phase change material, and hydronic cooling coil, etc. 

## Model Description
The new chiller object is able to include ten sets of performance curves and interpolate performance between speed levels. If a VS air-source chiller object (Coil:Chiller:AirSource:VariableSpeed) contains only set of performance curves, it would calculate cyclic loss and run time fraction to match a load. 

The user needs to input Rated Cooling Capacity, Rated Evaporator Water Flow Rate, at a selected nominal speed level. They are used to scale the performances of a specific unit and correlate with the actual loop flow rate. Except these two fields, all other capacity and flow rate inputs at individual speed levels should be directly obtained from Reference Unit catalog data, specific to an actual unit. The user also needs to define the rated evaporator inlet water/glycol temperature and the condenser air inlet temperature, where the rated cooling capacity is obtained. 

The Rated Cooling Capacity at a Selected Nominal Speed Level contains the rated capacity.  The rated Cooling capacity is used to determine a capacity scaling factor, as compared to the Reference Unit capacity at the nominal speed level. 

CapacityScaleFactor=(Rated Cooling Capacity)/(Reference Unit Cooling Capacity@Nominal Speed Level)

And then, this scaling factor is used to determine capacities at the rated condition for other speed levels, as below: 

Rated Cooling Capacity@Speed Level(x)=CapacityScaleFactor×Reference Unit Cooling Capacity@Speed Level(x)

The Rated Volumetric Water Flow Rate is used to determine an internal scaling factor, and calculate resultant flow rates in the parent objects, as follows:

WaterFlowScaleFactor=(Rated Volumetric Water Flow Rate)/(Reference Unit Vol Water Flow Rate@Nominal Speed Level×CapacityScaleFactor)

And the loop volumetric water flow rates at various speed levels in a parent object are calculated as below:

Loop Volumetric Water Flow Rate@Speed Level(x)=WaterFlowScaleFactor×Reference Unit Vol Water Flow Rate@Speed Level(x)×CapacityScaleFactor

If WaterFlowScaleFactor is equal to unity, the loop flow rate becomes the design flow rate of the Reference Unit (after scaled by the rated chilling capacity). The Rated Volumetric Water Flow Rate is introduced here to correlate with the actual flow rate in the water loop, in case that it differs from the design specification. Certainly, it is recommended that the Rated Volumetric Water Flow Rate is selected in the way that WaterFlowScaleFactor is unity, so as to get more accurate representations from the performance curves. 

Performance curves:
This object includes 5 curve objects at each individual speed level. 
1)  Cooling capacity modifier curve (function of temperature).
2)  Cooling capacity modifier curve (function of water flow fraction).
3)  Energy input ratio (EIR) modifier curve (function of temperature).
4)  Energy input ratio (EIR) modifier curve (function of water flow fraction).
5)  Part load fraction correlation (function of part load ratio) 

The flow fraction modifier curves, i.e. Curves 2) and 4), are used as placeholders, to account for off-design flow rates if needed. If the manufacturer doesn’t provide off-design performances, we can simply use a default modification multiplier of 1.0. At the lowest speed, Curve 5) will be used to account for the part-load condition. 

1) Cooling capacity modifier curve (function of temperature)

The Cooling capacity modifier as a function of temperature curve is a biquadratic curve with two independent variables: water temperature entering the evaporator coil, and the air dry bulb temperature entering the outdoor condenser. The output of this curve is multiplied by the rated cooling capacity at the speed, to give the cooling capacity at the specific entering water and air temperatures at which the chiller unit is operating (i.e., at temperatures different from the rating point temperatures). 

CapTempModFac=a+b×EWT+c×EWT<sup>2</sup>+d*T<sub>cond,i</sub>+e×T<sub>cond,i</sub><sup>2</sup>+f×T<sub>cond,i</sub>×EWT

Where
EWT = entering water temperature, °C

T<sub>cond,i</sub> = dry-bulb of air entering the condenser coil, °C

a-f = regression curve-fit coefficients

2) Cooling capacity modifier curve (function of water flow fraction)

CapWaterFlowModFac=a+b×ff<sub>w</sub>+c×ff<sub>w</sub><sup>2</sup>  + d×ff<sub>w</sub><sup>3</sup>

where
ff<sub>w</sub> = actual water mass flow rate/design water mass flow rate, at one speed level
 
Design Water Mass Flow Rate@Speed Level(x)= Reference Unit Water Mass Flow Rate@Speed Level(x)×CapacityScaleFactor

a-d = regression curve-fit coefficients, if no data available for the correction, the user can simply put a = 1.0, and the other coefficients as 0.0.

3) Energy input ratio (EIR) modifier curve (function of temperature)

The energy input ratio modifier curve as a function of temperature is a biquadratic curve with two independent variables: entering water temperature to the evaporator and dry-bulb temperature of air entering the condenser. The output of this curve is multiplied by the rated EIR at the speed (inverse of the rated COP), to give the EIR at the specific entering water and outdoor air temperatures at which the chilled water coil unit is operating (i.e. at temperatures different from the rating point temperatures). 

EIRTempModFac=a+b×EWT+c×EWT<sup>2</sup>+d*T<sub>cond,i</sub>+e×T<sub>cond,i</sub><sup>2</sup>+f×T<sub>cond,i</sub>×EWT

a-f = regression curve-fit coefficients

4) Energy input ratio (EIR) modifier curve (function of water flow fraction)

EIRWaterFlowModFac=a+b×ff<sub>w</sub>+c×ff<sub>w</sub><sup>2</sup>  + d×ff<sub>w</sub><sup>3</sup>

a-d = regression curve-fit coefficients, if no data available for the correction, the user can simply put a = 1.0, and the other coefficients as 0.0.

5) Part load fraction correlation (function of part load ratio)

This field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the unit as a function of the part load ratio (PLR, heating load/steady-state heating capacity for Speed 1), 

PartLoadFrac=a+b×PLR+c×PLR<sup>2</sup>+d×PLR<sup>3</sup>

And 

RTF = (PLR/PartLoadFrac) = runtime fraction of the heating coil

The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling. The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor runs continuously for the simulation timestep). For PLR values between 0 and 1 (0 <= PLR < 1), the following rules apply: 
PLF >= 0.7 and PLF >= PLR 
If PLF < 0.7, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the coil is defined as PLR/PLF. If PLF < PLR, the runtime fraction of the coil is limited to 1.0. A typical part load fraction correlation would be: 

PLF=0.85+0.15×PLR

Lowest Speed Operation:

The lowest speed operation of a VS chiller is similar to a single speed chiller coil. The chilled water capacity of the unit is calculated as follows: 

Q<sub>cooling,1</sub>=Rated Cooling Capacity@Speed Level(1)×CapTempModFac<sub>1</sub>×CapWaterFlowModFac<sub>1 </sub>

And the EIR is calculated as:

EIR<sub>1</sub>=1.0/(Reference Unit COP@Speed(1)) EIRTempModFac<sub>1</sub>×EIRWaterFlowModFac<sub>1 </sub>


And the power consumption is, 

Power<sub>1</sub>=Q<sub>cooling,1</sub>*EIR<sub>1</sub>*RTF

The cooling capacity calculated above may or may not include the impact of pump heat. For this reason, the user input evaporator Pump Heat Included in Rated cooling Capacity and Rated COP is used to determine the total water heating capacity including pump heat. 

IF(evaporator Pump Included In Rated Capacity )

Q<sub>cooling,tot,1</sub>=Q<sub>cooling,1</sub>

ELSE

Q<sub>cooling,tot,1</sub>=Q<sub>cooling,1</sub>-P<sub>pump</sub>*Fraction<sub>pumptowater</sub>

ENDIF

The power including the compressor and outdoor fan is then calculated based on two additional inputs provided by the user. The first input specifies if the evaporator pump heat is included in the rated cooling capacity and rated COP. If the evaporator pump heat is included in the rated cooling capacity and COP, then evaporator pump power must be subtracted from the DX cooling coil power calculated above to determine the compressor power

IF(Evaporator Pump Included In Rated COP)

P<sub>comp,1</sub>=Power_1-P_(pump,1)

ELSE

P<sub>comp,1</sub>=Power<sub>1</sub>

ENDIF

The fraction of the actual water mass flow to the design water mass flow rate is calculated: 

ff<sub>w,1</sub>=actual water mass flow rate/ (Reference Unit  Water Mass Flow Rate@Speed Level(1)×CapacityScaleFactor) 

The runtime fraction of the DX coil compressor is calculated as the ratio of the compressor part load ratio to the part load fraction correlation entered by the user. 

Higher Speed Operation: 

At the speed level between the lowest and the highest, there is no part-load loss, i.e. RTF= 1.0. A parameter of speed ratio (SpeedRatio) is used to define the capacity partition between Speed x-1 and Speed x. 

The design water flow rate at the speed ratio is given as following:

DesignWaterFlowRateSpeedRatio=Reference Unit  Water Mass Flow Rate@Speed Level(x-1)×CapacityScaleFactor × (1 – SpeedRatio)+Reference Unit  Water Mass Flow Rate@Speed Level(x) × CapacityScaleFactor × SpeedRatio 

And the fractions of water flow is given:
ff<sub>w,x-1</sub>= ff<sub>w,x</sub>= actual water mass flow rate/DesignWaterFlowRateSpeedRatio

The heating capacities and EIRs at Speed x-1 and Speed x are given: 

Q<sub>cooling,x-1</sub>=Rated Cooling Capacity@Speed Level(x-1)*CapTempModFac<sub>x-1</sub>*CapWaterFlowModFac<sub>x-1</sub> 

Q<sub>cooling,x</sub>=Rated Cooling Capacity@Speed Level(x)*CapTempModFac<sub>x</sub>*CapWaterFlowModFac<sub>x</sub> 

EIR<sub>x-1</sub>=1.0/(Reference Unit  COP@Speed(x-1))×EIRTempModFac<sub>x-1</sub>×EIRWaterFlowModFac<sub>x-1</sub> 

EIR<sub>x</sub>=1.0/(Reference Unit  COP@Speed(x))×EIRTempModFac<sub>x</sub>×EIRWaterFlowModFac<sub>x</sub>

The cooling capacity at the corresponding speed ratio is:

Q<sub>cooling,SpeedRatio</sub>=(1.0-SpeedRatio)×Q<sub>cooling,x-1</sub>+SpeedRatio×Q<sub>cooling,x</sub>

And the power consumption is 

Power<sub>SpeedRatio</sub>=(1.0-SpeedRatio)*Q<sub>cooling,x-1</sub>*EIR<sub>x-1</sub>+SpeedRatio*Q<sub>cooling,x</sub>*EIR<sub>x</sub>

If the speed reaches the highest level, the speed ratio becomes 1.0, and Speed x represents the highest speed.  

For the higher speeds, calculations of the evaporator cooling capacity and compressor power,  i.e. how to add or subtract the pump power, are the same as the lowest speed. 

Finally, the evaporator water outlet temperature is calculated based on the total chilled water capacity and the actual water/glycol mass flow rate.

T<sub>evaporator,out,water</sub>=T<sub>evaportor,in,water</sub>+Q<sub>cooling,tot</sub>/(m ̇<sub>evap,water</sub>  * Cp<sub>water</sub> )

The crankcase heater power, and the leaving air conditions for the outdoor coil are calculated the same way as they are for the DX cooling coil model. 


The Coil:Chiller:AirSource:VariableSpeed contains four fields to take a grid signal input,

A grid signal schedule represents an electric signal, e.g. hourly price. A max speed level should be given to limit the chiller capacity, if a grid-responsvie operation is required. When the max speed is defined a zero, the chiller will shut off. A lower bound and a upper bound define the grid-responsive operation logic. When the grid signal falls between the two boundary values, the grid responsive operation will run. 

Coil:Chiller:AirSource:VariableSpeed Sizing

The variable-speed chiller will be sized by its parent object, i.e. CoilSystem:IntegratedHeatPump:AirSource. 

## Implementation
The codes were implemented to VariableSpeedCoils.cc and VariableSpeedCoils.hh. 

## Testing

One new example file has been added to demonstrate the new features. The example file takes the ``HeatPumpVSAS.idf`` file and  modifies it to "ASIHPIceStorage.idf". 

## Documentation

Some inline code documentation has been added, and additional documentation will be added later.

## IDD Changes and Transition

A new input example is shown below. 
  Coil:Chiller:AirSource:VariableSpeed,

    VSChillerCoil,               !- Name
    1.0,                      !- Number of Speeds {dimensionless}
    1.0,                      !- Nominal Speed Level {dimensionless}
    autosize,                  !- Rated Cooling Capacity {W}
    -1.1,                   !- Rated Evaporator Inlet Water Temperature {C}
    35.0,                   !- Rated Condenser Inlet Air Temperature {C}
    0.00063,                     !- Rated Evaporator Water Flow Rate {m3/s}
    No,                      !- Evaporator Pump Power Included in Rated COP
    No,                      !- Evaporator Pump Heat Included in Rated Heating Capacity and Rated COP
    0.1,                     !- Fraction of Evaporator Pump Heat to Water
    ChillerInletNode,        !- Evaporator Water Inlet Node Name 
    ChillerWaterOutletNode,  !- Evaporator Water Outlet Node Name 
    100.0,                   !- Crankcase Heater Capacity {W}
    5.0,                     !- Maximum Ambient Temperature for Crankcase Heater Operation {C}
    HPWHPLFFPLR,             !- Part Load Fraction Correlation Curve Name
	GRIDSUMMER,				 ! - grid signal schedule
	10.0,				     ! - Low bound to apply grid responsive control
	1000.0,                  ! - High boundar to apply grid responsive control
	0.0,                     ! - max speed when appy grid responsive control
    9555.0,                  !- Rated Chilled Capacity at Speed 1 {W}
    2.36,                     !- Rated Chilled COP at Speed 1 {W/W}
    1.58,                !- Speed 1 Reference Unit Rated Air Flow Rate {m3/s}
    0.00063,                !- Speed 1 Reference Unit Rated Water Flow Rate {m3/s}, 10 gpm
    15.0,                    !- Speed 1 Reference Unit Water Pump Input Power At Rated Conditions {W}
    ChillerCAPFTemp,     !- Speed 1 Total Chilled Capacity Function of Temperature Curve Name
    ConstantCubic,           !- Speed 1 Total Chilled Capacity Function of Water Flow Fraction Curve Name
    ChillerEIRFTemp,     !- Speed 1 EIR Function of Temperature Curve Name
    ConstantCubic,           !- Speed 1 EIR Function of Water Flow Fraction Curve Name
    ,                  !- Rated Chilled Capacity at Speed 2 {W}
    ,                     !- Rated Chilled COP at Speed 2 {W/W}
    ,                !- Speed 2 Reference Unit Rated Air Flow Rate {m3/s}
    ,                !- Speed 2 Reference Unit Rated Water Flow Rate {m3/s}
    ,                    !- Speed 2 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    ,     !- Speed 2 Total Chilled Capacity Function of Temperature Curve Name
    ,           !- Speed 2 Total Chilled Capacity Function of Water Flow Fraction Curve Name
    ,     !- Speed 2 EIR Function of Temperature Curve Name
    ,           !- Speed 2 EIR Function of Water Flow Fraction Curve Name
    ,                 !- Rated Chilled Capacity at speed 3 {W}
    ,                     !- Rated Chilled COP at Speed 3 {W/W}
    ,                !- Speed 3 Reference Unit Rated Air Flow Rate {m3/s}
    ,                !- Speed 3 Reference Unit Rated Water Flow Rate {m3/s}
    ,                    !- Speed 3 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    ,     !- Speed 3 Total Chilled Capacity Function of Temperature Curve Name
    ,           !- Speed 3 Total Chilled Capacity Function of Water Flow Fraction Curve Name
    ,     !- Speed 3 EIR Function of Temperature Curve Name
    ,           !- Speed 3 EIR Function of Water Flow Fraction Curve Name
    ,                 !- Rated Chilled Capacity at Speed 4 {W}
    ,                     !- Rated Chilled COP at Speed 4 {W/W}
    ,                !- Speed 4 Reference Unit Rated Air Flow Rate {m3/s}
    ,                !- Speed 4 Reference Unit Rated Water Flow Rate {m3/s}
    ,                    !- Speed 4 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    ,     !- Speed 4 Total Chilled Capacity Function of Temperature Curve Name
    ,           !- Speed 4 Total Chilled Capacity Function of Water Flow Fraction Curve Name
    ,     !- Speed 4 EIR Function of Temperature Curve Name
    ,           !- Speed 4 EIR Function of Water Flow Fraction Curve Name
    ,                 !- Rated Chilled Capacity at Speed 5 {W}
    ,                     !- Rated Chilled COP at Speed 5 {W/W}
    ,                !- Speed 5 Reference Unit Rated Air Flow Rate {m3/s}
    ,                !- Speed 5 Reference Unit Rated Water Flow Rate {m3/s}
    ,                    !- Speed 5 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    ,     !- Speed 5 Total Chilled Capacity Function of Temperature Curve Name
    ,           !- Speed 5 Total Chilled Capacity Function of Water Flow Fraction Curve Name
    ,     !- Speed 5 EIR Function of Temperature Curve Name
    ,           !- Speed 5 EIR Function of Water Flow Fraction Curve Name
    ,                 !- Rated Chilled Capacity at Speed 6 {W}
    ,                     !- Rated Chilled COP at Speed 6 {W/W}
    ,                !- Speed 6 Reference Unit Rated Air Flow Rate {m3/s}
    ,                !- Speed 6 Reference Unit Rated Water Flow Rate {m3/s}
    ,                    !- Speed 6 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    ,     !- Speed 6 Total Chilled Capacity Function of Temperature Curve Name
    ,           !- Speed 6 Total Chilled Capacity Function of Water Flow Fraction Curve Name
    ,     !- Speed 6 EIR Function of Temperature Curve Name
    ,           !- Speed 6 EIR Function of Water Flow Fraction Curve Name
    ,                 !- Rated Chilled Capacity at Speed 7 {W}
    ,                     !- Rated Chilled COP at Speed 7 {W/W}
    ,                !- Speed 7 Reference Unit Rated Air Flow Rate {m3/s}
    ,                !- Speed 7 Reference Unit Rated Water Flow Rate {m3/s}
    ,                    !- Speed 7 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    ,     !- Speed 7 Total Chilled Capacity Function of Temperature Curve Name
    ,           !- Speed 7 Total Chilled Capacity Function of Water Flow Fraction Curve Name
    ,     !- Speed 7 EIR Function of Temperature Curve Name
    ,           !- Speed 7 EIR Function of Water Flow Fraction Curve Name
    ,                 !- Rated Chilled Capacity at Speed 8 {W}
    ,                     !- Rated Chilled COP at Speed 8 {W/W}
    ,                !- Speed 8 Reference Unit Rated Air Flow Rate {m3/s}
    ,                !- Speed 8 Reference Unit Rated Water Flow Rate {m3/s}
    ,                    !- Speed 8 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    ,     !- Speed 8 Total Chilled Capacity Function of Temperature Curve Name
    ,           !- Speed 8 Total Chilled Capacity Function of Water Flow Fraction Curve Name
    ,     !- Speed 8 EIR Function of Temperature Curve Name
    ,           !- Speed 8 EIR Function of Water Flow Fraction Curve Name
    ,                 !- Rated Chilled Capacity at Speed 9 {W}
    ,                     !- Rated Chilled COP at Speed 9 {W/W}
    ,                !- Speed 9 Reference Unit Rated Air Flow Rate {m3/s}
    ,                !- Speed 9 Reference Unit Rated Water Flow Rate {m3/s}
    ,                    !- Speed 9 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    ,     !- Speed 9 Total Chilled Capacity Function of Temperature Curve Name
    ,           !- Speed 9 Total Chilled Capacity Function of Water Flow Fraction Curve Name
    ,     !- Speed 9 EIR Function of Temperature Curve Name
    ,           !- Speed 9 EIR Function of Water Flow Fraction Curve Name
    ,                 !- Rated Chilled Capacity at Speed 10 {W}
    ,                     !- Rated Chilled COP at Speed 10 {W/W}
    ,                !- Speed 10 Reference Unit Rated Air Flow Rate {m3/s}
    ,                !- Speed 10 Reference Unit Rated Water Flow Rate {m3/s}
    ,                    !- Speed 10 Reference Unit Water Pump Input Power At Rated Conditions {dimensionless}
    ,     !- Speed 10 Total Chilled Capacity Function of Temperature Curve Name
    ,           !- Speed 10 Total Chilled Capacity Function of Water Flow Fraction Curve Name
    ,     !- Speed 10 EIR Function of Temperature Curve Name
    ;           !- Speed 10 EIR Function of Water Flow Fraction Curve Name



IDD Objects (New):

Coil:Chiller:AirSource:VariableSpeed,

    \memo vairlable-speed air-source chiller cooling coil, air-to-water direct-expansion (DX)
    \memo system which includes a variable-speed chilled water coil, condenser air coil, evaporator water coil
    \memo electric compressor, and water pump. Part of a CoilSystem:IntegratedHeatPump:AirSource.
    \min-fields 33

    A1 , \field Name
        \required-field
        \type alpha
        \reference ChillerDXCoilsVariableSpeed
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
    N3 , \field Rated Chilled Water Capacity
        \required-field
        \type real
        \units W
        \note Chilled water capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
	    \autosizable	   
    N4 , \field Rated Evaporator Inlet Water Temperature
        \type real
        \units C
        \minimum> -100.0
        \default 8.0
        \note Evaporator inlet water temperature corresponding to rated coil performance
        \note (Cooling capacity, and COP).
    N5 , \field Rated Condenser Inlet Air Temperature
        \type real
        \units C
        \minimum> 10
        \default 35.0
        \note Condenser inlet air dry-bulb temperature corresponding to rated coil performance
        \note (Cooling capacity, and COP).  
    N6 , \field Rated Evaporator Water Flow Rate
        \type real
        \units m3/s
        \ip-units gal/min
        \minimum> 0
        \autocalculatable
        \note Evaporator water flow rate corresponding to rated coil performance
        \note (Cooling capacity and COP).
        \note Default is 4.487E-8 m3/s/W (0.208 gpm/MBH) of rated heating capacity when autocalculated.
        \note A warning message will be issued if the ratio of Rated Condenser Water Flow Rate
        \note to Cooling Capacity is less than 1.79405E-8 m3/s/W (0.083 gpm/MBH)
        \note or greater than 8.97024E-8 m3/s/W (0.417 gpm/MBH), but the simulation will continue.
    A2 , \field Evaporator Pump Power Included in Rated COP
        \type choice
        \key Yes
        \key No
        \default No
        \note Select Yes if the evaporator pump power is included in the rated COP. This choice field
        \note impacts the calculation of compressor electric power.
    A3 , \field Evaporator Pump Heat Included in Rated Cooling Capacity and Rated COP
        \type choice
        \key Yes
        \key No
        \default No
        \note Select Yes if the evaporator pump heat is included in the rated cooling capacity and
        \note rated COP. This choice field impacts the calculation of water cooling capacity.
    N7 , \field Fraction of Evaporator Pump Heat to Water
        \type real
        \minimum 0
        \maximum 1
        \default 0.2
        \note Fraction of pump heat transferred to the evaporator water. The pump is assumed
        \note to be located downstream of the evaporator.
    A4 , \field Evaporator Water Inlet Node Name
        \required-field
        \type node
        \note The node from which the DX coil evaporator draws its inlet water.
        \note This name should match the load side outlet node name in the associated
        \note storage tank object.
    A5 , \field Evaporator Water Outlet Node Name
        \required-field
        \type node
        \note The node to which the DX coil evaporator sends its outlet water.
        \note This name should match the load side inlet node name in the associated
        \note storage tank object.
    N8 , \field Crankcase Heater Capacity
        \type real
        \minimum 0
        \default 0
        \units W
        \note The compressor crankcase heater only operates when the dry-bulb temperature of air
        \note surrounding the compressor is below the Maximum Ambient Temperature for Crankcase
        \note Heater Operation and the DX coil is off.  The ambient temperature surrounding the
        \note compressor is set by the WaterHeater:HeatPump parent object (field Compressor Location).
    N9 , \field Maximum Ambient Temperature for Crankcase Heater Operation
        \type real
        \minimum 0
        \default 10
        \units C
        \note The compressor crankcase heater only operates when the dry-bulb temperature of air
        \note surrounding the compressor is below the Maximum Outdoor Temperature for Crankcase
        \note Heater Operation and the unit is off. The ambient temperature surrounding the
        \note compressor is set by the WaterHeater:HeatPump parent object (field Compressor Location).
    A6 , \field Part Load Fraction Correlation Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note Part Load Fraction Correlation (function of part load ratio) should be quadratic or cubic.
        \note Quadratic curve = a + b(PLR) + c(PLR)^2.
        \note Cubic curve = a + b(PLR) + c(PLR)^2 + d(PLR)^3.
        \note PLR = part load ratio (heating delivered/steady state heating capacity).
        \note Use curve coefficients of 1,0,0 or leave this field blank when neglecting performance impacts
        \note due to variations in part load ratio.
    A7 , \field Grid Signal Schedule Name
        \type object-list
        \object-list ScheduleNames
        \note This field refers to a grid signal pattern, e.g. price 
    N10, \field Lower Bound to Apply Grid Responsive Control
        \type real
        \units dimensionless
		\default 100.0
    N11, \field Upper Bound to Apply Grid Responsive Control
        \type real
        \units dimensionless
		\default -100.0
    N12, \field Max Speed Level During Grid-Responsive Control
        \type real
        \units dimensionless   
		\default 10.0
    N13, \field Rated Water Cooling Capacity at Speed 1
        \required-field
        \type real
        \units W
        \minimum> 0
        \note Cooling capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
    N14, \field Rated Water Cooling COP at Speed 1
        \type real
        \units W/W
        \minimum> 0
        \default 3.2
        \note Cooling coefficient of performance at the rated inlet water and air temperatures,
        \note rated evaporator inlet water temperature,and rated water flow rate.
        \note Can optionally include evaporator pump power.
    N15, \field Speed 1 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
        \required-field
    N16, \field Speed 1 Reference Unit Rated Water Flow Rate
        \units m3/s
        \ip-units gal/min
        \type real
        \minimum 0
        \required-field
    N17, \field Speed 1 Reference Unit Water Pump Input Power At Rated Conditions
        \units W
        \type real
        \minimum 0
        \required-field
    A8, \field Speed 1 Total Cooling Capacity Function of Temperature Curve Name
        \required-field
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A9, \field Speed 1 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        \required-field
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    A10, \field Speed 1 EIR Function of Temperature Curve Name
        \required-field
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A11, \field Speed 1 EIR Function of Water Flow Fraction Curve Name
        \required-field
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    N18, \field Rated Water Cooling Capacity at Speed 2
        \type real
        \units W
        \minimum> 0
        \note Cooling capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
    N19, \field Rated Water Cooling COP at Speed 2
        \type real
        \units W/W
        \minimum> 0
        \default 3.2
        \note Cooling coefficient of performance at the rated inlet water and air temperatures,
        \note rated evaporator inlet water temperature,and rated water flow rate.
        \note Can optionally include evaporator pump power.
    N20, \field Speed 2 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
    N21, \field Speed 2 Reference Unit Rated Water Flow Rate
        \units m3/s
        \ip-units gal/min
        \type real
        \minimum 0
    N22, \field Speed 2 Reference Unit Water Pump Input Power At Rated Conditions
        \units W
        \type real
        \minimum 0
    A12, \field Speed 2 Total Cooling Capacity Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A13, \field Speed 2 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    A14, \field Speed 2 EIR Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A15, \field Speed 2 EIR Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    N23, \field Rated Water Cooling Capacity at Speed 3
        \type real
        \units W
        \minimum> 0
        \note Cooling capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
    N24, \field Rated Water Cooling COP at Speed 3
        \type real
        \units W/W
        \minimum> 0
        \default 3.2
        \note Cooling coefficient of performance at the rated inlet water and air temperatures,
        \note rated evaporator inlet water temperature,and rated water flow rate.
        \note Can optionally include evaporator pump power.
    N25, \field Speed 3 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
    N26, \field Speed 3 Reference Unit Rated Water Flow Rate
        \units m3/s
        \ip-units gal/min
        \type real
        \minimum 0
    N27, \field Speed 3 Reference Unit Water Pump Input Power At Rated Conditions
        \units W
        \type real
        \minimum 0
    A16, \field Speed 3 Total Cooling Capacity Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A17, \field Speed 3 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    A18, \field Speed 3 EIR Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A19, \field Speed 3 EIR Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    N28, \field Rated Water Cooling Capacity at Speed 4
        \type real
        \units W
        \minimum> 0
        \note Cooling capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
    N29, \field Rated Water Cooling COP at Speed 4
        \type real
        \units W/W
        \minimum> 0
        \default 3.2
        \note Cooling coefficient of performance at the rated inlet water and air temperatures,
        \note rated evaporator inlet water temperature,and rated water flow rate.
        \note Can optionally include evaporator pump power.
    N30, \field Speed 4 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
    N31, \field Speed 4 Reference Unit Rated Water Flow Rate
        \units m3/s
        \ip-units gal/min
        \type real
        \minimum 0
    N32, \field Speed 4 Reference Unit Water Pump Input Power At Rated Conditions
        \units W
        \type real
        \minimum 0
    A20, \field Speed 4 Total Cooling Capacity Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A21, \field Speed 4 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    A22, \field Speed 4 EIR Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A23, \field Speed 4 EIR Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    N33, \field Rated Water Cooling Capacity at Speed 5
        \type real
        \units W
        \minimum> 0
        \note Cooling capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
    N34, \field Rated Water Cooling COP at Speed 5
        \type real
        \units W/W
        \minimum> 0
        \default 3.2
        \note Cooling coefficient of performance at the rated inlet water and air temperatures,
        \note rated evaporator inlet water temperature,and rated water flow rate.
        \note Can optionally include evaporator pump power.
    N35, \field Speed 5 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
    N36, \field Speed 5 Reference Unit Rated Water Flow Rate
        \units m3/s
        \ip-units gal/min
        \type real
        \minimum 0
    N37, \field Speed 5 Reference Unit Water Pump Input Power At Rated Conditions
        \units W
        \type real
        \minimum 0
    A24, \field Speed 5 Total Cooling Capacity Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A25, \field Speed 5 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    A26, \field Speed 5 EIR Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A27, \field Speed 5 EIR Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    N38, \field Rated Water Cooling Capacity at Speed 6
        \type real
        \units W
        \minimum> 0
        \note Cooling capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
    N39, \field Rated Water Cooling COP at Speed 6
        \type real
        \units W/W
        \minimum> 0
        \default 3.2
        \note Cooling coefficient of performance at the rated inlet water and air temperatures,
        \note rated evaporator inlet water temperature,and rated water flow rate.
        \note Can optionally include evaporator pump power.
    N40, \field Speed 6 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
    N41, \field Speed 6 Reference Unit Rated Water Flow Rate
        \units m3/s
        \ip-units gal/min
        \type real
        \minimum 0
    N42, \field Speed 6 Reference Unit Water Pump Input Power At Rated Conditions
        \units W
        \type real
        \minimum 0
    A28, \field Speed 6 Total Cooling Capacity Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A29, \field Speed 6 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    A30, \field Speed 6 EIR Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A31, \field Speed 6 EIR Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    N43, \field Rated Water Cooling Capacity at Speed 7
        \type real
        \units W
        \minimum> 0
        \note Cooling capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
    N44, \field Rated Water Cooling COP at Speed 7
        \type real
        \units W/W
        \minimum> 0
        \default 3.2
        \note Cooling coefficient of performance at the rated inlet water and air temperatures,
        \note rated evaporator inlet water temperature,and rated water flow rate.
        \note Can optionally include evaporator pump power.
    N45, \field Speed 7 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
    N46, \field Speed 7 Reference Unit Rated Water Flow Rate
        \units m3/s
        \ip-units gal/min
        \type real
        \minimum 0
    N47, \field Speed 7 Reference Unit Water Pump Input Power At Rated Conditions
        \units W
        \type real
        \minimum 0
    A32, \field Speed 7 Total Cooling Capacity Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A33, \field Speed 7 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    A34, \field Speed 7 EIR Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A35, \field Speed 7 EIR Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    N48, \field Rated Water Cooling Capacity at Speed 8
        \type real
        \units W
        \minimum> 0
        \note Cooling capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
    N49, \field Rated Water Cooling COP at Speed 8
        \type real
        \units W/W
        \minimum> 0
        \default 3.2
        \note Cooling coefficient of performance at the rated inlet water and air temperatures,
        \note rated evaporator inlet water temperature,and rated water flow rate.
        \note Can optionally include evaporator pump power.
    N50, \field Speed 8 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
    N51, \field Speed 8 Reference Unit Rated Water Flow Rate
        \units m3/s
        \ip-units gal/min
        \type real
        \minimum 0
    N52, \field Speed 8 Reference Unit Water Pump Input Power At Rated Conditions
        \units W
        \type real
        \minimum 0
    A36, \field Speed 8 Total Cooling Capacity Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A37, \field Speed 8 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    A38, \field Speed 8 EIR Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A39, \field Speed 8 EIR Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    N53, \field Rated Water Cooling Capacity at Speed 9
        \type real
        \units W
        \minimum> 0
        \note Cooling capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
    N54, \field Rated Water Cooling COP at Speed 9
        \type real
        \units W/W
        \minimum> 0
        \default 3.2
        \note Cooling coefficient of performance at the rated inlet water and air temperatures,
        \note rated evaporator inlet water temperature,and rated water flow rate.
        \note Can optionally include evaporator pump power.
    N55, \field Speed 9 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
    N56, \field Speed 9 Reference Unit Rated Water Flow Rate
        \units m3/s
        \ip-units gal/min
        \type real
        \minimum 0
    N57, \field Speed 9 Reference Unit Water Pump Input Power At Rated Conditions
        \units W
        \type real
        \minimum 0
    A40, \field Speed 9 Total Cooling Capacity Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A41, \field Speed 9 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    A42, \field Speed 9 EIR Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A43, \field Speed 9 EIR Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    N58, \field Rated Water Cooling Capacity at Speed 10
        \type real
        \units W
        \minimum> 0
        \note Cooling capacity at the rated inlet water temperatures, rated condenser inlet
        \note air temperature, and rated water flow rate.
        \note Can optionally include evaporator pump heat.
    N59, \field Rated Water Cooling COP at Speed 10
        \type real
        \units W/W
        \minimum> 0
        \default 3.2
        \note Cooling coefficient of performance at the rated inlet water and air temperatures,
        \note rated evaporator inlet water temperature,and rated water flow rate.
        \note Can optionally include evaporator pump power.
    N60, \field Speed 10 Reference Unit Rated Air Flow Rate
        \units m3/s
        \type real
        \minimum 0
    N61, \field Speed 10 Reference Unit Rated Water Flow Rate
        \units m3/s
        \ip-units gal/min
        \type real
        \minimum 0
    N62, \field Speed 10 Reference Unit Water Pump Input Power At Rated Conditions
        \units W
        \type real
        \minimum 0
    A44, \field Speed 10 Total Cooling Capacity Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A45, \field Speed 10 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow
    A46, \field Speed 10 EIR Function of Temperature Curve Name
        \type object-list
        \object-list BivariateFunctions
        \note Table:Lookup object can also be used
        \note curve = a + b*ewt + c*ewt**2 + d*db + e*db**2 + f*db*ewt
        \note db = condenser entering dry-bulb temperature (C)
        \note ewt = water entering temperature seen by the evaporator (C)
    A47; \field Speed 10 EIR Function of Water Flow Fraction Curve Name
        \type object-list
        \object-list UnivariateFunctions
        \note Table:Lookup object can also be used
        \note quadratic curve = a + b*ffw + c*ffw**2
        \note cubic curve = a + b*ffw + c*ffw**2 + d*ffw**3
        \note ffw = Fraction of the full load Water Flow