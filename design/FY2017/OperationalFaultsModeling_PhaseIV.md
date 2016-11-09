Modeling HVAC Operational Faults – Phase IV
================

 **Rongpeng Zhang, Tianzhen Hong**
 **Lawrence Berkeley National Laboratory**

 - Original Date: Nov 7, 2016
 

## Justification for New Feature ##

Most of the buildings, either new or old, have operational faults in the sensors, controllers, meters, equipment and systems. Being able to model and simulate these faults and their impact on energy performance of buildings is crucial to improve accuracy of building simulations and to support the retrofit of buildings.

To date, the main practitioner use of EnergyPlus has been for new construction design. With the new high priority attached by DOE to retrofit and improved operation of existing buildings, there is a need to extend the capabilities of EnergyPlus to model existing buildings, including faulty operation:

-	Retrofit analysis: starts with calibrated simulation; the ability to estimate the severity of common faults is expected to improve the accuracy and transparency of the calibrated model and hence increase the accuracy of analyzing different retrofit measures.  
-	Commissioning providers can use the fault models to demonstrate the savings to be expected from fixing faults found in retro-commissioning
-	Support for building operation by using the calibrated model, including unfixed faults, as a real-time reference model to detect, and verify the diagnosis of, newly occurring faults.

The users in these cases will be practitioners, not the building modeling experts, so it is planned to implement the fault models using conventional EnergyPlus objects rather than the EMS, which, in any case, could only be used to implement a subset of the faults models.   Additional use cases are noted in Enhancement List document 'General_2009_08.doc'.   

A literature review of operational faults in buildings was carried out and a number of common HVAC equipment faults were identified. These faults were then ranked for both complexity of implementation and the severity of the associated energy penalty. Based on the ranking, the following types of occurring faults have been implemented in EnergyPlus in Phase I to III:

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
5.	Cooling Tower Scaling (under review)
6.	Chiller Supply Water Temperature Sensor Offset (under review)
7.	Condenser Supply Water Temperature Sensor Offset (under review)
8.	Coil Supply Air Temperature Sensor Offset  (under review)

Phase IV in FY17 will focus more on the faults in the plant loop, particularly on the fouling of water-based cooling and heating equipment. Fouling fault occurs when deposits get clogged, usually caused by poor water quality and treatment. It can considerably reduce the overall heat transfer coefficient and thus cause deficiency of equipment efficiency and waste of energy. 

We propose to add the following fouling fault models to EnergyPlus: 

1.	Fouling of hot-water boilers, 
2.	Fouling of water-cooled chillers, 
3.	Fouling of evaporative coolers.

This is a continuous effort from current and previous FYs to build a list of common and impactful fault models to EnergyPlus to improve modeling of existing buildings for retrofit or operational improvements to save energy.


## Comments and Reply ##
N/A

## Overview ##

### Fault: Fouling of Hot-water Boilers ###

#### Symptom ####

The fouling of the hot water boilers changes the nominal capacity of the boiler. This further impacts the boiler operations by changing the part load ratio and the related operation/performance parameters.

#### Model inputs ####

Boiler name, reference reduction factor of nominal capacity, name of availability schedule (on/off schedule to indicate when faults are applicable), name of severity schedule (normalized schedule to indicate the degree of faults)

#### Approach ####

The fault applies to the hot water boiler model described by the object Boiler:HotWater. It does not apply to the steam boilers which do not have water-based heat exchangers.

Note that operational faults only affect the HVAC operations, not the system design. Therefore, the fault model will only be applied at real weather simulations instead of the sizing and warm-up simulations.

### Fault: Fouling of Water-cooled Chillers ###

#### Symptom ####

The fouling of the water-cooled condenser at chillers changes the nominal capacity of the chiller. This further impacts the chiller operations by changing the part load ratio and the related operation/performance parameters.

#### Model inputs ####

Chiller name, reference reduction factor of nominal capacity, name of availability schedule (on/off schedule to indicate when faults are applicable), name of severity schedule (normalized schedule to indicate the degree of faults)

#### Approach ####

The fault applies to a number of chiller types that can have water-cooled condensers, namely:
-	Chiller:Electric,
-	Chiller:Electric:EIR
-	Chiller:Electric:ReformulatedEIR,
-	Chiller:ConstantCOP,
-	Chiller:EngineDriven,
-	Chiller:CombustionTurbine.

The fault does not apply to the absorption chillers that do not have water-based heat exchangers.

### Fault: Fouling of Evaporative Coolers ###

#### Symptom ####

The fouling of the indirect wet-coil evaporative coolers changes the design effectiveness of the tube. This further impacts the cooler operations by changing the part load ratio and the related operation/performance parameters.

#### Model inputs ####

Evaporative cooler name, reference reduction factor of nominal efficiency, name of availability schedule (on/off schedule to indicate when faults are applicable), name of severity schedule (normalized schedule to indicate the degree of faults)

#### Approach ####

The fault applies to the wetted coil evaporative cooler described by object EvaporativeCooler:Indirect:WetCoil, where the cooling water is sprayed directly on the tubes. The fault does not apply to direct evaporative coolers or the dry coil indirect evaporative coolers where there is no water-cooled coil.


## IDD Object (New) ##
New objects will be created for the proposed fault types, namely: 
-	FaultModel:Fouling:Boiler
-	FaultModel:Fouling:Chiller
-	FaultModel:Fouling:EvaporativeCooler


## Testing/Validation/Data Source(s) ##
Comparing simulation results with and without faults will be performed verify the new fault models. 

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

