Condenser hot gas reheat model for DX Cooling Coil System
================

**Lixing Gu**

**Florida Solar Energy Center**

 - Third Draft
 - Discussed in the sizing conference call. No more comments
 - Revision Date: 10/24/19
 - Second Draft
 - Add Conference call discussion and E-mail communications from Brent. Also add simulation logic for multispeed coils
 - Revision Date: 10/14/19
 - First Draft 
 - Original Date: 9/24/19
 

## Justification for New Feature ##

Many equipment manufacturers offer dehumidification reheat using condenser hot gas in rooftop, split direct expansion air handling units (DX AHU), and direct expansion dedicated outdoor air system (DX DOAS) products.

In applications where active humidity control is used to control either discharge air or space air relative humidity, dehumidification reheat is required to prevent sensible overcooling. Dehumidification by conventional means such as electric resistance or fossil fuel combustion heaters is costly and energy-inefficient. To confront this challenge manufacturers have developed schemes to take heat from the DX refrigeration system condenser and use it to provide what is "free" reheat. These schemes are typically referred to as "hot gas reheat". Beyond simply using hot gas for reheat, some manufacturers have devised controls to allow dehumidification and hot gas reheat at times of high sensible load, low sensible load, or no sensible load.

Currently EnergyPlus does not provide modeling capabilities for hot gas reheat. Addition of this feature to EnergyPlus would allow engineers to evaluate the energy-efficient humidity control options. An EnergyPlus model for this feature would need to model not just the use of hot gas for free reheat, but also controls for these  dehumidification modes. 

## E-mail and  Conference Call Conclusions ##

###Conference Call on 10/23/19

People attended:

Lawrence Scheier; Michael Witte; Ricahrd Raustad; Edwind Lee; Jim Spielbauer; Nagappan Chidambaram; Tiejun Wu, Jason DeGraw

No more comments

###Conference Call on 10/09/19

People attended:

Lawrence Scheier; Michael Witte; Ricahrd Raustad; Edwind Lee; Jim Spielbauer; Nagappan Chidambaram; Tiejun Wu

The proposed simulation logic covers a single speed coil only. The logic should also cover multispeed coils.

The existing coil object may be revised to accomodate 3 operation modes by adding new fields or modifying existing fields, when the new coil object is available. 

###E-mail communication between Brent and me

####My response

Brent:

Thanks for your comments. Here are my answers to your questions.

1.	Coil:Heating:Desuperheater

Luis raised the same question in August. Tiejun made a reply, available in https://github.com/NREL/EnergyPlus/issues/7421. Since the reply has figures, it is better for you to take a look over there, if you have not read it.

The NFP was written later. I assume the reply justified new object needs already, so that I did not want to repeat it again in the NFP.

2.	New generation DX coil model

As discussed in the conference call, the new model will be built on the new generation DX coil model. The coil object may be revised later.

3.	Next generation humidity control

I agree. We may need to add fields in the existing object of ZoneHVAC:EquipmentList with priority and fraction. I think this issue is beyond the new feature and we can discuss it for future development.

4.	Was there any discussion of water coil controller
No discussion of water coil controller.

Thanks.

Gu

####Brent's comments

From: Brent Griffith <Brent.Griffith@EnergyArchmage.com> 
Sent: Monday, October 14, 2019 11:33 AM
To: 'Lixing Gu' <gu@fsec.ucf.edu>; 'Lawrence Scheier' <lscheier@sei-associates.com>; 'Michael J Witte' <mjwitte@gard.com>; 'Richard Raustad' <rraustad@fsec.ucf.edu>; 'Edwin Lee' <leeed2001@gmail.com>; 'Spielbauer, Jim' <JSPIELBAUER@trane.com>; 'Chidambaram, Nagappan' <Nagappan.Chidambaram@trane.com>
Subject: RE: Sizing, etc. call this Wednesday

Sorry I missed the call last week.

Gu, I was surprised the NFP makes no mention of the existing Coil:Heating:Desuperheater model and its approach.  I guess it is too simplified but I would think the NFP should discuss its shortcomings and why a new object is needed. 

What about the new generation DX coil model, shouldn’t this build off of that work?

As an aside, I think next generation humidity control systems will also need improved humidity setpoint managers based on zone moisture loads.  The current ones are accurate only when there is a one air system inlet node into the zone and 100% of the air entering the zone is from the central air handler.  For more complex zone configurations, such as PIU air terminals, combined Zone HVAC and DOAS, more than one air system, etc., the central air humidity setpoints are not being calculated correctly because the primary air flow rate is much lower than the total air system inlet flows into a zone. 

Was there any discussion of water coil controller?

Brent


## Overview ##

The goal of this new feature is to provide a DX cooling coil capability to meet all possible demand with different sensible and latent loads by using different mode opeation in a single cooling coil. In  other words, the coil itself can meet sensible and latent loads simultaneously wihtout any additional help.
 
Using a simple space thermostat and humidistat input, the Humidi-MiZer Adaptive Dehumidification system (shown in Figure 1) changes the refrigerant flow by adjusting the position of the refrigerant solenoid valves. There are three modes of operation: Normal, Sub-Cooling and Hot Gas ReHeat.

### Operation modes ###

Normal Mode (HGSV closed, LLSV open)

When there is a call for cooling only, the dehumidification system is inactive and the refrigerant circulates per a
typical packaged system.

Sub-Cooling Mode (HGSV closed, LLSV closed)

During part load conditions when the room temperature and humidity are above the setpoint, the unit will initiate
the sub-cooling mode of operation; a call for cooling and dehumidification. The end result is a conditioned space
that is cooled and significantly more dehumidified, but not over-cooled. This also helps eliminate short cycling
of the rooftop unit and improves space temperature and humidity control.

Hot Gas ReHeat Mode 
(HGSV open, LLSV closed)

When there is a call for dehumidification without a call for cooling, a portion of the hot gas from the compressor
bypasses the condenser coil and is fed into the liquid line. The air is cooled and dehumidified as it flows across the evaporator and is then reheated to neutral conditions by the Humidi-MiZer coil. When used on an Applied Rooftop
unit the Humidi-MiZer® System becomes modulating. 

![Figure 1 Schematic of dehumidimizer operation modes](HumidiMizer.PNG)

**Figure 1 Schematic of dehumidimizer operation modes (Extracted from Carrier 2)**

### Operation Algorithm

We will create 3 sets of performance curves. The first set repesents normal operation mode, which is the same as existing DX cooling coil. The second set represents subcooling operation with LLSV fully closed. The third set represents hot gas reheat mode with HGSV full open.

#### Subcooling operation

When requested system load SHR is between normal operation SHR and subcooling operation SHR, the coil will operate a portion of the time at two modes to meet the request. The linear iterperation of opening ratio between two modes is applied, so that coil will act as an ideal coil to provide exactly requested sensible and latent outputs to ensure zone is controlled at given thermal and humidity setpoints. The opening ratio is equivalent to openess of liquid line solenoid valve.   

#### Reheat operation

When requested system load SHR is between normal operation SHR and reheat mode operation SHR, the coil will operation a portion of the time at two modes to meet the request. The linear iterperation of opening ratio between two modes is applied, so that coil will act as an ideal coil to provide exactly requested sensible and latent outputs to ensure zone is controlled at given thermal and humidity setpoints. The opening ratio is equivalent to  openess of hot gas solenoid valve.

## Approach ##

### A new coil object

Coil:Cooling:DX:AdaptiveDehumidifcation

The new object is close to Coil:Cooling:DX:TwoStageWithHumidityControlMode. It requires 3 sets of performance curves corresponding to 3 operation modes, respectively. Since Reheat Mode works in a narrow operation range, it has a field with maximum outdoor dry-bulb temperature to restrict the reheat mode operation. The minimum outdoor air dry-bulb temperature should be the thermal setpoint. Since this value is available, there is no need to be as input. 

This new object will be called by two parent objects: CoilSystem:Cooling:DX and AirLoopHVAC:UnitarySystem. 

### Pseudocode

The new object will require 3 performance curve sets to represent coil performance: normal operation, subcool operation and reheat mode.

Note: There are 3 objects of CoilPerformance:DX:Cooling to provide coil performance data, including performance curves. Here are proposed requirement for 3 objects.

1. Normal operation

This operation does not require special inputs. Operation SHR is calculated based on rated SHR and ADT method. There is no need to input SHR function names.

2. Subcooling operation at LLSV fully closed 

This operation requires inputs of SHR functions of temperature and flow fraction with Fields A9 and A10.

3. Reheat mode at HGSV fully open

This operation also requires inputs of SHR functions of temperature and flow fraction with Fields A9 and A10.

### Control logic

####Nomenclatures

SenLoad = System sensible load

LatLoad = System latent load

SenOut_{Normal} = System sensible output at normal operation at full capacity

LatOut_{Normal} = System latent output at normal operation at full capacity

SenOut_{Sub} = System sensible output at subcooling operation at full capacity with LLSV fully closed

LatOut_{Sub} = System latent output at subcooling operation at full capacity with LLSV fully closed

SenOut_{Reheat} = System sensible output at reheat operation at full capacity with HGSV fully open

LatOut_{Reheat} = System latent output at reheat operation at full capacity with HGSV fully open

SHR_{Load} = System load SHR as Sensible load / (Sensible load + Latent Load)

SHR_{Normal} = System output SHR at normal operation

SHR_{Sub} = System output SHR at subcooling operation with LLSV fully closed

SHR_{Reheat} = System ouput SHR at normal operation with HGSV fully closed

r_{Sub} = Subcooling ratio between subcooling and normal operation

r_{Reheat} = Reheat ratio between hot gas reheat and normal operation

Tdb_{max} = Maximum Outdoor Dry-Bulb Temperature for Reheat Mode Operation

####Simulation logic

1.	No latent control

Perform normal operation with SHR_{Normal}

2.	Latent control

Calculate coil load SHR_{load} = Sensible load / (Sensible load + Latent load)

	a.	If SHR_{Load} >= SHR_{Normal}

	Perform normal operation to meet sensible load, adjusted by PLR for a single capacity equipment or at Speed 1 for a multispeed coil

	SenLoad = SenOut * PLR

    If SenOut < SenLoad, go to existing multispeed coil operation

	b.	Else if (SHR_{Normal} > SHR_{Load} >= SHR_{Sub})

	Perform combination between normal and subcooling operation

	SHR_{Load} = SHR_{Sub} * r_{Sub} + SHR_{Normal} * (1 - r_{Sub})

	SenLoad = [SenOut_{Sub} * r_{Sub} + SenOut_{Normal} * (1 - r_{Sub})] * PLR

    If SenLoad > [SenOut_{Sub} * r_{Sub} + SenOut_{Normal} * (1 - r_{Sub})], go to "Multispeed subcooling operation"

	c.	Else // Case SHR_{Load} < SHR_{Sub}

	if (TdbOut <= Tdb_max) Then

		Perform combination between normal and reheat operation

		SHR_{Load} = SHR_{Reheat} * r_{Reheat} + SHR_{Normal} * (1 - r_{Reheat})

		SenLoad = [SenOut_{Reheat} * r_{Reheat} + SenOut_{Normal} * (1 - r_{Reheat})] * PLR

	Else
		Perform subcooling operation

		SenLoad = SenOut_{Sub} * PLR
   
        If SenLoad > SenOut_{Sub}, go to "Multispeed reheat operation"

	End IF

#####Multispeed subcooling operation

The same SHR_{Load} will be applied to both speeds first. Then combined capacities between subcooling and normal operation will be calculated to ensure sensible load is met.

Set SHR_{load} for two consecutive speeds: i and i+1

	SHR_{Load} = SHR_{Sub_i} * r_{Sub_i} + SHR_{Normal_i} * (1 - r_{Sub_i})

	SHR_{Load} = SHR_{Sub_{i+1}} * r_{Sub_{i+1}} + SHR_{Normal_{i+1}} * (1 - r_{Sub_{i+1}})

Set Combined sensible capacity for two consecutive speeds: i and i+1

	SenOut_{CombSub_i} = [SenOut_{Sub_i} * r_{Sub_i} + SenOut_{Normal_i} * (1 - r_{Sub_i})]

	SenOut_{CombSub_{i+1}} = [SenOut_{Sub_{i+1}} * r_{Sub_{i+1}} + SenOut_{Normal_{i+1}} * (1 - r_{Sub_{i+1}})]

Perform calculation

if SenOut_{CombSub_i} < SenLoad <= SenOut_{CombSub_{i+1}} Then

	SenLoad = SenOut_{CombSub_{i+1}} * SpeedRatio_{i+1} + SenOut_{CombSub_i} * [ 1 - SpeedRatio_{i+1} ]

Else

	Go to next speed

End If

#####Multispeed reheat operation

The same SHR_{Load} will be applied to both speeds first. Then combined capacity between reheat mode and normal operationwill be calculated to ensure sensible load is met.


Set SHR_{load} for two consecutive speeds: i and i+1

	SHR_{Load} = SHR_{Reheat_i} * r_{Reheat_i} + SHR_{Normal_i} * (1 - r_{Reheat_i})

	SHR_{Load} = SHR_{Reheat_{i+1}} * r_{Reheat_{i+1}} + SHR_{Normal_{i+1}} * (1 - r_{Reheat_{i+1}})

Set Combined sensible capacity for two consecutive speeds: i and i+1

	SenOut_{CombReheat_i} = [SenOut_{Reheat_i} * r_{Reheat_i} + SenOut_{Normal_i} * (1 - r_{Reheat_i})]

	SenOut_{CombReheat_{i+1}} = [SenOut_{Reheat_{i+1}} * r_{Reheat_{i+1}} + SenOut_{Normal_{i+1}} * (1 - r_{Reheat_{i+1}})]

Perform calculation

if SenOut_{CombReheat_i} < SenLoad <= SenOut_{CombReheat_{i+1}} Then

	SenLoad = SenOut_{CombReheat_{i+1}} * SpeedRatio_{i+1} + SenOut_{CombReheat_i} * [ 1 - SpeedRatio_{i+1} ]

Else

	Go to next speed

End If

### Implementation

We will revise 3 module for this new feature.

####DXCoils

Modify an existing function of GetCoilsInput

	Read new coil object

Creat a new function of CalcAdaptiveHumification

	Calcualte coil output based on system load and input conditions

####UnitarySystem

	Modify existing function to call DXCoils

####HVACDXSystems

	Modify existing function to call DXCoils

###Sizing

There are 3 fields with autosize choices in CoilPerformance:DX:Cooling

####Normal operation

Gross Rated Total Cooling Capacity: Use existing method

Gross Rated Sensible Heat Ratio: Use existing method

Rated Air Flow Rate: Use existing method
 
####Subcooling operation at LLSV fully closed 

Gross Rated Total Cooling Capacity: Use existing method

Gross Rated Sensible Heat Ratio: Select SHR from input curve objects with rated conditions

Rated Air Flow Rate: Use existing method

####Reheat mode at HGSV fully open

Gross Rated Total Cooling Capacity: Use existing method

Gross Rated Sensible Heat Ratio: Select SHR from input curve objects with rated conditions

Rated Air Flow Rate: Use existing method

## Testing/Validation/Data Sources ##

Unit test will be performed to ensure numerical values are calculated properly.

## Input Output Reference Documentation ##

Examples to use the nex coil object

CoilSystem:Cooling:DX,

  	CoilSystem:Cooling:DX,
    DOAS DX Cooling System,  !- Name
    ,                        !- Availability Schedule Name
    DOAS Supply Fan Outlet,  !- DX Cooling Coil System Inlet Node Name
    DOAS Cooling Coil Outlet,!- DX Cooling Coil System Outlet Node Name
    DOAS Cooling Coil Outlet,!- DX Cooling Coil System Sensor Node Name
    Coil:Cooling:DX:AdaptiveDehumidifcation,  !- Cooling Coil Object Type
    DOAS Cooling Coil,       !- Cooling Coil Name
    Multimode,               !- Dehumidification Control Type
    Yes,                     !- Run on Sensible Load
    Yes;                     !- Run on Latent Load

Coil:Cooling:DX:AdaptiveDehumidifcation,

  	Coil:Cooling:DX:AdaptiveDehumidifcation,
    DehumidiMizer Cooling Coil,       !- Name
    ,                        !- Availability Schedule Name
    DehumidiMizer Supply Fan Outlet,  !- Air Inlet Node Name
    DehumidiMizer Cooling Coil Outlet,!- Air Outlet Node Name
    ,                        !- Crankcase Heater Capacity {W}
    ,                        !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}
    CoilPerformance:DX:Cooling,  !- Normal Operation Coil Performance Object Type
    DehumidiMizer Standard Perf,    !- Normal Operation Coil Performance Name
    CoilPerformance:DX:Cooling,  !- Subcooling Operation Coil Performance Object Type
    DehumidiMizer Subcooled Perf,  !- Subcooling Operation Coil Coil Performance Name
    CoilPerformance:DX:Cooling,  !- Reheat Mode Coil Performance Object Type
    DehumidiMizer Reheat Perf,     !- Reheat Mode Coil Performance Name
    28.0,  !- Maximum Outdoor Dry-Bulb Temperature for Reheat Mode Operation
    ,                        !- Supply Water Storage Tank Name
    ,                        !- Condensate Collection Water Storage Tank Name
    0;                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}

 CoilPerformance:DX:Cooling,

  	CoilPerformance:DX:Cooling,
    DehumidiMizer Standard Perf,    !- Name
    autosize,                !- Gross Rated Total Cooling Capacity {W}
    autosize,                !- Gross Rated Sensible Heat Ratio
    3,                       !- Gross Rated Cooling COP {W/W}
    autosize,                !- Rated Air Flow Rate {m3/s}
    0.5,                     !- Fraction of Air Flow Bypassed Around Coil
    DehumidiMizer Std Cap-FT, !- Total Cooling Capacity Function of Temperature Curve Name
    DehumidiMizer Std Cap-FF,     !- Total Cooling Capacity Function of Flow Fraction Curve Name
    DehumidiMizer Std EIR-FT, !- Energy Input Ratio Function of Temperature Curve Name
    DehumidiMizer Std EIR-FF,     !- Energy Input Ratio Function of Flow Fraction Curve Name
    DehumidiMizer Std PLF,        !- Part Load Fraction Correlation Curve Name
    ,                        !- Nominal Time for Condensate Removal to Begin {s}
    ,                        !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}
    0;                       !- Maximum Cycling Rate {cycles/hr}

  	CoilPerformance:DX:Cooling,
    DehumidiMizer Subcooled Perf,    !- Name
    autosize,                !- Gross Rated Total Cooling Capacity {W}
    autosize,                !- Gross Rated Sensible Heat Ratio
    2.8,                       !- Gross Rated Cooling COP {W/W}
    autosize,                !- Rated Air Flow Rate {m3/s}
    0.5,                     !- Fraction of Air Flow Bypassed Around Coil
    DehumidiMizer Subcool Cap-FT, !- Total Cooling Capacity Function of Temperature Curve Name
    DehumidiMizer Subcool Cap-FF,     !- Total Cooling Capacity Function of Flow Fraction Curve Name
    DehumidiMizer Subcool EIR-FT, !- Energy Input Ratio Function of Temperature Curve Name
    DehumidiMizer Subcool EIR-FF,     !- Energy Input Ratio Function of Flow Fraction Curve Name
    DehumidiMizer Subcool PLF,        !- Part Load Fraction Correlation Curve Name
    ,                        !- Nominal Time for Condensate Removal to Begin {s}
    ,                        !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}
    0,                       !- Maximum Cycling Rate {cycles/hr}
    ,                        !- Latent Capacity Time Constant
    ,                        !- Condenser Air Inlet Node Name
    ,                        !- Evaporative Condenser Effectiveness
    ,                        !- Evaporative Condenser Air Flow Rate
    ,                        !- Evaporative Condenser Pump Rated Power Consumption
    DehumidiMizer Subcool SHR Temp Curve Bame,  !- Sensible Heat Ratio Function of Temperature Curve Name
    DehumidiMizer Subcool SHR Flow Curve Bame;  !- Sensible Heat Ratio Function of Flow Fraction Curve Name


  	CoilPerformance:DX:Cooling,
    DehumidiMizer Reheat Perf,    !- Name
    autosize,                !- Gross Rated Total Cooling Capacity {W}
    autosize,                !- Gross Rated Sensible Heat Ratio
    2.5,                       !- Gross Rated Cooling COP {W/W}
    autosize,                !- Rated Air Flow Rate {m3/s}
    0.5,                     !- Fraction of Air Flow Bypassed Around Coil
    DehumidiMizer Reheat Cap-FT, !- Total Cooling Capacity Function of Temperature Curve Name
    DehumidiMizer Reheat Cap-FF,     !- Total Cooling Capacity Function of Flow Fraction Curve Name
    DehumidiMizer Reheat EIR-FT, !- Energy Input Ratio Function of Temperature Curve Name
    DehumidiMizer Reheat EIR-FF,     !- Energy Input Ratio Function of Flow Fraction Curve Name
    DehumidiMizer Reheat PLF,        !- Part Load Fraction Correlation Curve Name
    ,                        !- Nominal Time for Condensate Removal to Begin {s}
    ,                        !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}
    0,                       !- Maximum Cycling Rate {cycles/hr}
    ,                        !- Latent Capacity Time Constant
    ,                        !- Condenser Air Inlet Node Name
    ,                        !- Evaporative Condenser Effectiveness
    ,                        !- Evaporative Condenser Air Flow Rate
    ,                        !- Evaporative Condenser Pump Rated Power Consumption
    DehumidiMizer Reheat SHR Temp Curve Bame,  !- Sensible Heat Ratio Function of Temperature Curve Name
    DehumidiMizer Reheat SHR Flow Curve Bame;  !- Sensible Heat Ratio Function of Flow Fraction Curve Name


## Input Description ##

The secition includes a new object and modified 2 objects shown in the idd, and input samples shown in the idf. 

### Coil:Cooling:DX:AdaptiveDehumidifcation in the idd

	Coil:Cooling:DX:AdaptiveDehumidifcation,
        \memo Direct expansion (DX) cooling coil and condensing unit (includes electric compressor
        \memo and condenser, heat exchanger, and fan), single stage with humidity control mode (e.g. sub-cool or hot gas
        \memo reheat). Optional inputs for moisture evaporation from wet coil when compressor
        \memo cycles off with continuous fan operation. Requires three sets of performance
        \memo data, see CoilPerformance:DX:Cooling, to represent conventional cooling, sub-cooling with heat exchanger and hot gas reheat, respectrively.
  	\min-fields 10
  	A1 , \field Name
       \required-field
       \type alpha
       \reference CoolingCoilsDX
       \reference CoolingCoilsDXMultiModeOrSingleSpeed
       \reference DesuperHeatingCoilSources
       \reference AFNCoilNames
  	A2 , \field Availability Schedule Name
       \note Availability schedule name for this system. Schedule value > 0 means the system is available.
       \note If this field is blank, the system is always available.
       \type object-list
       \object-list ScheduleNames
  	A3 , \field Air Inlet Node Name
       \required-field
       \type node
  	A4 , \field Air Outlet Node Name
       \required-field
       \type node
  	N1 , \field Crankcase Heater Capacity
       \type real
       \minimum 0.0
       \default 0.0
       \units W
       \ip-units W
  	N2 , \field Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation
       \type real
       \minimum 0.0
       \default 10.0
       \units C
       \default 1
  	A5 , \field Normal Operation Coil Performance Object Type
       \required-field
       \type choice
       \key CoilPerformance:DX:Cooling
  	A6 , \field Normal Operation Coil Performance Name
       \required-field
       \type object-list
       \object-list CoilPerformanceDX
  	A7 , \field Subcooling Operation Coil Performance Object Type
       \type choice
       \key CoilPerformance:DX:Cooling
  	A8 , \field Subcooling Operation Coil Coil Performance Name
       \type object-list
       \object-list CoilPerformanceDX
  	A9 , \field Reheat Mode Coil Performance Object Type
       \type choice
       \key CoilPerformance:DX:Cooling
  	A10, \field Reheat Mode Coil Performance Name
       \type object-list
       \object-list CoilPerformanceDX
  	N3,  \field Maximum Outdoor Dry-Bulb Temperature for Reheat Mode Operation
       \type real
       \default 25.0
       \units C 
  	A11, \field Supply Water Storage Tank Name
       \type object-list
       \object-list WaterStorageTankNames
  	A12, \field Condensate Collection Water Storage Tank Name
       \type object-list
       \object-list WaterStorageTankNames
  	N4,  \field Minimum Outdoor Dry-Bulb Temperature for Compressor Operation
       \type real
       \default -25.0
       \units C
  	N5,  \field Basin Heater Capacity
       \type real
       \units W/K
       \minimum 0.0
       \default 0.0
       \note This field is only used for Condenser Type = EvaporativelyCooled and for periods
       \note when the basin heater is available (field Basin Heater Operating Schedule Name).
       \note For this situation, the heater maintains the basin water temperature at the basin heater
       \note setpoint temperature when the outdoor air temperature falls below the setpoint temperature.
       \note The basin heater only operates when the DX coil is off.
  	N6,  \field Basin Heater Setpoint Temperature
       \type real
       \units C
       \minimum 2.0
       \default 2.0
       \note This field is only used for Condenser Type = EvaporativelyCooled.
       \note Enter the outdoor dry-bulb temperature when the basin heater turns on.
  	A15; \field Basin Heater Operating Schedule Name
       \type object-list
       \object-list ScheduleNames
       \note This field is only used for Condenser Type = EvaporativelyCooled.
       \note Schedule values greater than 0 allow the basin heater to operate whenever the outdoor
       \note air dry-bulb temperature is below the basin heater setpoint temperature.
       \note If a schedule name is not entered, the basin heater is allowed to operate
       \note throughout the entire simulation.

### Revised CoilSystem:Cooling:DX and AirLoopHVAC:UnitarySystem

A new choice of cooling coil type will be added to allow these two parent objects can call the proposed new cooling coil. The new choice will be highlighted in red

####CoilSystem:Cooling:DX

	CoilSystem:Cooling:DX,
        \memo Virtual container component that consists of a DX cooling coil and its associated
        \memo controls. This control object supports several different types of DX cooling coils
        \memo and may be placed directly in an air loop branch or outdoor air equipment list.
        \min-fields 7
   	A1 , \field Name
        \required-field
        \reference CoolingCoilSystemName
        \type alpha
        \reference-class-name validBranchEquipmentTypes
        \reference validBranchEquipmentNames
        \reference-class-name validOASysEquipmentTypes
        \reference validOASysEquipmentNames
	....
   	A6,  \field Cooling Coil Object Type
        \type choice
        \required-field
        \key Coil:Cooling:DX:SingleSpeed
        \key CoilSystem:Cooling:DX:HeatExchangerAssisted
        \key Coil:Cooling:DX:TwoSpeed
        \key Coil:Cooling:DX:TwoStageWithHumidityControlMode
        \key Coil:Cooling:DX:VariableSpeed
        \key Coil:Cooling:DX:SingleSpeed:ThermalStorage
<span style="color:red">\key Coil:Cooling:DX:AdaptiveDehumidifcation</span>

   	A7,  \field Cooling Coil Name
        \required-field
        \type object-list
        \object-list CoolingCoilsDX
        \object-list CoolingCoilsDXVariableSpeed
	.....

####AirLoopHVAC:UnitarySystem

	AirLoopHVAC:UnitarySystem,
       \memo AirloopHVAC:UnitarySystem is a generic HVAC system type that allows any
       \memo configuration of coils and/or fan. This object is a replacement of other
       \memo AirloopHVAC objects. This object can be used in outdoor air systems,
       \memo outdoor air units, air loops, and as zone equipment if desired.
       \min-fields 14
  	A1,  \field Name
       \required-field
       \type alpha
       \reference DOAToZonalUnit
       \note Unique name for the Unitary System.
       \reference-class-name validBranchEquipmentTypes
       \reference validBranchEquipmentNames
       \reference-class-name validOASysEquipmentTypes
       \reference validOASysEquipmentNames
       \reference ZoneEquipmentNames
	.....
  	A14, \field Cooling Coil Object Type
       \type choice
       \key Coil:Cooling:DX:SingleSpeed
       \key Coil:Cooling:DX:TwoSpeed
       \key Coil:Cooling:DX:MultiSpeed
       \key Coil:Cooling:DX:VariableSpeed
       \key Coil:Cooling:DX:TwoStageWithHumidityControlMode
       \key Coil:Cooling:DX:SingleSpeed:ThermalStorage
       \key CoilSystem:Cooling:DX:HeatExchangerAssisted
       \key Coil:Cooling:WaterToAirHeatPump:ParameterEstimation
       \key Coil:Cooling:WaterToAirHeatPump:EquationFit
       \key Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit
       \key Coil:Cooling:Water
       \key Coil:Cooling:Water:DetailedGeometry
       \key CoilSystem:Cooling:Water:HeatExchangerAssisted
       \key Coil:UserDefined
<span style="color:red">\key Coil:Cooling:DX:AdaptiveDehumidifcation</span>

       \note Enter the type of cooling coil if included in the unitary system.
 	A15, \field Cooling Coil Name
       \type object-list
       \object-list CoolingCoilsDX
       \object-list CoolingCoilsDXMultiSpeed
       \object-list CoolingCoilsDXVariableSpeed
       \object-list CoolingCoilsWaterToAirHP
       \object-list CoolingCoilsWaterToAirVSHP
       \object-list CoolingCoilsWater
       \object-list UserDefinedCoil
       \note Enter the name of the cooling coil if included in the unitary system.
	......

## Outputs Description ##

insert text

## Engineering Reference ##

insert text

## Example File and Transition Changes ##

An example file will be created.

No transition is needed.

## References ##

[Carrier-1, HUMIDI-MIZER® ADAPTIVE DEHUMIDIFICATION SYSTEM FOR APPLIED ROOFTOP UNITS](https://www.utcccs-cdn.com/hvac/docs/1001/Public/01/04-581048-01.pdf)

[Carrier-2, Humid-Mizer Adaptive Dehumidification System](https://www.utcccs-cdn.com/hvac/docs/1001/Public/02/04-811-70007.pdf)

