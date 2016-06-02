Modeling HVAC Operational Faults – Phase III
================

 **Rongpeng Zhang, Tianzhen Hong.**
 **Lawrence Berkeley National Laboratory**

 - Original Date: May 25, 2016
 

## Justification for New Feature ##

Most of the buildings, either new or old, have operational faults in the sensors, controllers, meters, equipment and systems. Being able to model and simulate these faults and their impact on energy performance of buildings is crucial to improve accuracy of building simulations and to support the retrofit of buildings.

To date, the main practitioner use of EnergyPlus has been for new construction design. With the new high priority attached by DOE to retrofit and improved operation of existing buildings, there is a need to extend the capabilities of EnergyPlus to model existing buildings, including faulty operation:

-	Retrofit analysis: starts with calibrated simulation; the ability to estimate the severity of common faults is expected to improve the accuracy and transparency of the calibrated model and hence increase the accuracy of analyzing different retrofit measures.  
-	Commissioning providers can use the fault models to demonstrate the savings to be expected from fixing faults found in retro-commissioning
-	Support for building operation by using the calibrated model, including unfixed faults, as a real-time reference model to detect, and verify the diagnosis of, newly occurring faults.

The users in these cases will be practitioners, not the building modeling experts, so it is planned to implement the fault models using conventional EnergyPlus objects rather than the EMS, which, in any case, could only be used to implement a subset of the faults models.   Additional use cases are noted in Enhancement List document 'General_2009_08.doc'.   

A literature review of operational faults in buildings was carried out and a number of common HVAC equipment faults were identified. These faults were then ranked for both complexity of implementation and the severity of the associated energy penalty. Based on the ranking, four types of occurring faults have been implemented in EnergyPlus in Phase I and Phase II, including:

1.	Economizer-related Faults
	a.	damper leakage
	b.	temperature sensor offset 
	c.	enthalpy sensor offset 
	d.	humidity sensor offset 
	e.	pressure sensor offset 
2.	Thermostat/Humidistat Offset 
3.	Coil Fouling 
	a.	heating coils 
	b.	cooling coils
4.	Air Filter Fouling 

Compared with Phase I and II that focused on the faults related with air loop, Phase III will include the faults in the plant loop. Four new types of fault will be implemented in Phase III, namely
1.	Chiller Supply Water Temperature Sensor Offset
2.	Condenser Supply Water Temperature Sensor Offset
3.	Cooling Tower Scaling
4.	Coil Supply Air Temperature Sensor Offset 


## Overview ##

### Fault: Chiller Supply Water Temperature Sensor Offset ###
#### Symptom ####
The chiller supply water temperature readings deviate from the actual water temperature levels due to sensor offset at the evaporator outlet. This can lead to incorrect chiller supply water temperature, and thus the inappropriate and inefficient chiller operations.
#### Model inputs ####
Chiller name, reference offset value, name of availability schedule (on/off schedule to indicate when faults are applicable), name of severity schedule (normalized schedule to indicate the degree of faults)
#### Approach ####
The fault applies to a number of chiller types, namely:
-	Chiller:Electric,
-	Chiller:Electric:EIR,
-	Chiller:Electric:ReformulatedEIR
-	Chiller:ConstantCOP,
-	Chiller:EngineDriven,
-	Chiller:CombustionTurbine,
-	Chiller:Absorption,
-	Chiller:Absorption:Indirect.

These chillers can have different flow modes:
-	"ConstantFlow" for constant pumping with flow controlled by chiller to operate at full design flow rate.  
-	"LeavingSetpointModulated" for variable pumping with flow controlled by chiller to vary flow to target a leaving temperature setpoint.
-	"NotModulated" for either variable or constant pumping with flow controlled by the external plant system.

**(a)** 
For the chillers with “ConstantFlow” and “NotModulated”, local control is provided by resetting the leaving water temperature. The actual evaporator outlet water temperature value at faulty operations can be obtained via:

<div>$$ T_{evap-o,f} = T_{evap-o,ff} \pm \Delta T $$</div>

where 
<span>$ T_{evap-o,f} $</span> 	evaporator outlet temperature in the faulty case (actual value)
<span>$ T_{evap-o,ff} $</span> 	evaporator outlet temperature in the fault-free case (reading value)
<span>$ \Delta T $</span> 	difference between the temperature reading and the actual temperature

Then the evaporator capacity can be calculated with:

<div>$$ Q_{evap,f} = m_{evap} \times C_p \times (T_{evap-i} - T_{evap-o,f} ) $$</div>

where 
<span>$ m_{evap}$</span> 	evaporator water flow rate (design value, actual value)
<span>$ Q_{evap,f} $</span> 	actual evaporator capacity in the faulty case
<span>$ T_{evap-o,f} $</span> 	evaporator outlet temperature value in the faulty case (actual value)
<span>$ T_{evap-i} $</span> 	evaporator inlet temperature value 

**(b)**
For the variable flow chillers with internal water flow rate controls to target a leaving temperature setpoint (type "LeavingSetpointModulated"), the actual evaporator outlet water temperature value at faulty operations can be obtained via:

<div>$$ T_{evap-o,f} = T_{evap-o,ff} \pm \Delta T $$</div>

where 
<span>$ T_{evap-o,f} $</span> 	evaporator outlet temperature in the faulty case (actual value)
<span>$ T_{evap-o,ff} $</span> 	evaporator outlet temperature in the fault-free case (reading, design value)
<span>$ \Delta T $</span> 	difference between the temperature reading and the actual temperature

The water flow rate at faulty operations can be obtained via:

<div>$$ m_{evap,f} = Q_{evap,ff} / C_p \times (T_{evap-i} - T_{evap-o,ff} ) $$</div>

where 
<span>$ m_{evap,f} $</span> 	evaporator water flow rate in the faulty case (actual value)
<span>$ Q_{evap,ff} $</span> 	evaporator capacity in the fault-free case (required value)
<span>$ Q_{evap,f} $</span> 	actual evaporator capacity in the faulty case (actual value)
<span>$ T_{evap-o,ff} $</span> 	evaporator outlet temperature in the fault-free case (reading, design value)
<span>$ T_{evap-i} $</span> 	evaporator inlet temperature value 
<span>$ \Delta T $</span> 	difference between the temperature reading and the actual temperature

Then the evaporator capacity can be calculated with:

<div>$$ Q_{evap,f} = m_{evap,f} \times C_p \times (T_{evap-i} - T_{evap-o,f} ) $$</div>
where 
<span>$ m_{evap,f}	 $</span> evaporator water flow rate in the faulty case (actual value)
<span>$ Q_{evap,f}	 $</span> actual evaporator capacity in the faulty case (actual value)
<span>$ T_{evap-o,f} $</span> 	evaporator outlet temperature value in the faulty case (actual value)
<span>$ T_{evap-i} $</span> 	evaporator inlet temperature value 

Note that for both the constant and variable flow chillers, actual evaporator outlet temperature value T_{evap-o,f} is used for the chiller efficiency calculations.

Also note that operational faults only affect the HVAC operations, not the system design. Therefore, the fault model will only be applied at real weather simulations instead of the sizing and warm-up simulations. 


### Fault: Condenser Supply Water Temperature Sensor Offset ###
#### Symptom ####
The condenser supply water temperature readings deviate from the actual water temperature levels due to sensor offset at the condenser inlet. Because this is usually used as the condenser loop temperature setpoint, the fault may affect the actual performance of cooling tower and condenser. It can results in inappropriate tower operations such as fan and pump cycling and water bypass. 
#### Model inputs ####
Tower name, reference offset value, name of availability schedule (on/off schedule to indicate when faults are applicable), name of severity schedule (normalized schedule to indicate the degree of faults)
#### Approach ####
The fault applies to a number of tower types, namely:
-	CoolingTower:SingleSpeed
-	CoolingTower:TwoSpeed
-	CoolingTower:VariableSpeed
-	CoolingTower:VariableSpeed:MERKEL.

 The effect of an offset in a condenser supply water temperature sensor whose sole use is for calculation of the difference between the set-points and the actual values can be modeled as an equal and opposite offset: 

<div>$$ T_{tower-o,f} = T_{tower-o,ff} \pm \Delta T $$</div>
where 
<span>$ T_{tower-o,f} $</span> 	tower outlet temperature in the faulty case (actual value)
<span>$ T_{tower-o,ff}	 $</span> tower outlet temperature in the fault-free case (reading value)
<span>$ \Delta T	 $</span> difference between the temperature reading and the actual temperature

Note that the fault affects the tower in both the free convection cooling mode when fan is off and normal cooling mode when fan is on. 

Also note that if the faulty sensor temperature goes beyond the sensor bounds (e.g., min/max condenser loop temperature defined in object “CondenserLoop”, or the min/max setpoint values defined in object “SetpointManager:FollowOutdoorAirTemperature”), the predefined bound values will be used as the actual temperature.

### Fault: Cooling Tower Scaling ###
#### Symptom ####
The fault of scaling widely exists in the cooling tower operations. It occurs when deposits get clogged, usually caused by poor water quality and treatment. It is reported that the removal of scale deposits is one of the biggest expenses in the cooling tower maintenance. Scale deposits can reduce the overall heat transfer coefficient (UA), affecting both the tower effectiveness and energy efficiency.
#### Model inputs ####
Name of the tower with fault, name of availability schedule of the fault, name of the fan severity schedule describing the variations of overall heat transfer coefficient due to tower fouling 
#### Approach ####
The fault applies to a number of tower types, namely:
-	CoolingTower:SingleSpeed
-	CoolingTower:TwoSpeed
-	CoolingTower:VariableSpeed
-	CoolingTower:VariableSpeed:MERKEL.

The fault model allows the user to describe the fouling information in either of the two methods: FouledUARated or FoulingFactor. Using FouledUARated method, user specifies the value of UAfouled directly. Using FoulingFactor method user specifies air/water side fouling factor, and the UAfouled value is further calculated via: 

<div>$$ UA_{tower,f} = UA_{tower,ff} \times F $$</div>
where 
<span>$ UA_{tower,f} $</span> 	U-factor times area values in the faulty case 
<span>$ UA_{tower,ff} $</span> 	U-factor times area values in the fault-free case 
<span>$ F $</span>  	Factor describing the fouling severity

Note that the fouling severity factor is applied to both the normal operational case and the free convection cooling case. 


### Fault: Coil Supply Air Temperature Sensor Offset ###
#### Symptom ####
The coil supply air temperature readings deviate from the actual air temperature levels due to sensor offset at the coil outlet. Because coil outlet node is usually used as the setpoint node for coil control, the fault may affect the actual performance of the coils. It can results in inappropriate coil operations such as coil on/off mode and water-side flow rate control, and therefore affect the coil energy consumption. Since the coil outlet air temperature deviate from the design level, the operations and performance of other components (e.g., other coils) at the downstream may also be affected.

#### Model inputs ####
Name of the coil with fault, reference offset value, name of availability schedule (on/off schedule to indicate when faults are applicable), name of severity schedule (normalized schedule to indicate the degree of faults)

#### Approach ####
EnergyPlus can model a number of coil types, some of which are temperature-based control and the others are load-based control. The proposed fault model will be applied to the ones with temperature-based control, namely:
-	Coil:Heating:Electric
-	Coil:Heating:Electric:Multistage
-	Coil:Heating:Gas
-	Coil:Heating:Gas:Multistage
-	Coil:Heating:Steam
-	Coil:Heating:Desuperheater
-	Coil:Heating:Water
-	Coil:Cooling:Water
-	Coil:Cooling:Water:Detailedgeometry

 The effect of an offset in a coil supply air temperature sensor whose sole use is for calculation of the difference between the set-points and the actual values can be modeled as an equal and opposite offset: 

<div>$$ T_{coil-o,f} = T_{coil-o,ff} \pm \Delta T $$</div>
where 
<span>$ T_{coil-o,f} $</span> 	coil outlet temperature in the faulty case (actual value)
<span>$ T_{coil-o,ff} $</span> 	coil outlet temperature in the fault-free case (reading value)
<span>$ \Delta T $</span> 	difference between the temperature reading and the actual temperature

Note that the fault only affect the coil operations, but doesn’t affect the coil sizing calculations. So it is possible that the faulty sensor temperature will lead to a heat transfer rate that goes beyond the maximum coil capacity. In this case, the maximum coil capacity will be used to reversely calculate the actual supply air temperature.

Also note that “Coil:Heating:Water”, “Coil:Cooling:Water”, and “Coil:Cooling:Water:Detailedgeometry” are controlled via “Controller:WaterCoil”, while the other coil types are controlled with an internal “Temperature Setpoint Node”. These two types may need to be handled separately in the fault model implementation.

## IDD Object (New) ##
New objects will be created for the proposed fault types, namely: 
-	FaultModel:TemperatureSensorOffset:ChillerSupplyWater,
-	FaultModel:TemperatureSensorOffset:CondenserSupplyWater,
-	FaultModel:TemperatureSensorOffset:CoilSupplyAir,
-	FaultModel:Fouling:tower.

## Testing/Validation/Data Source(s) ##
Comparing simulation results with and without faults will be performed to ensure accuracy of the new features. 

## IDD Object(s) (Revised) ##
None

## Proposed Report Variables ##
None

## Proposed additions to Meters ##
None

## IO Ref (draft) ##
To be developed.

## EngRef (draft) ##
To be developed.

## Example File  ##
To be developed.

## Transition changes ##
N/A

## Other documents ##
N/A

## E-mail and  Conference Call Conclusions ##
N/A

## Reference ##
N/A

