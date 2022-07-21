Implement steam features
================

**Dareum Nam, NREL**

 - Initial NFP Original Date: 12/3/2020
 - Final NFP Revision Date: 7/21/2022
 

## Justification for New Feature ##

Steam heating for hot water loops is common in university campuses and cities like New York. Often, steam energy is transferred at the building to a hot water system via a heat exchanger. Currently users can make both steam and hot water systems, but cannot link them together. This forces modelers to use HW boilers/systems to approximate the steam systems, which isn’t accurate and reduces confidence in the energy model. The request for steam to water heat exchanger came from Bractlet. There have also been several upvotes on the new feature request from EnergyPlus Github. In addition, the current energyplus does not allow to use districtheating and LoadProfile:Plant in a steam loop.

## E-mail and Conference Call Conclusions ##

EnergyPlus Technicalities Call on 2/24/2021
- We only have one phase steam plant fluid modeling. There is a lot of room for improvement and basic validation of current steam plant.
- If we are going to add more complexity to remove assumptions, we need new ways to find those value; for example, how do we calculate the quality if we want to remove quality 0 & 1 assumption?
- New systems could use current assumption for now. And if CoolProp is implemented, the steam systems can be renewed with enthalpy-based system.

EnergyPlus Iteration Call on 2/2/2022
- Instead of a separate object (PlantLoadProfile:Steam), a few optional input fields for a steam loop should be added to the current object (PlantLoadProfile).

EnergyPlus Technicalities Call on 2/9/2022
- Currently, there is "steam" as a resource type. This should be changed to DistrictHeatingSteam.
- Current "DistrictHeating" should be "DistrictHeatingWater".
- "DistrictHeatingWater" and "DistrictHeatingSteam" are better than "DistrictHeating:Water" and "DistrictHeating:Steam" because full meter names are a combination of resource type, end-use, etc. which are joined by colons so another colon would be confusing.
- Keep these "DistrictHeatingWater" and "DistrictHeatingSteam" separate for output report since hot water and steam basically have difference source factors and emission factors.
- SteamEquipment should go on the same meter as this (DistrictHeatingSteam)
- As for Steam to Water Heat Exchanger, we stick with two explicit objects because it's better for user standpoint.

## Overview ##

The current steam loop in EnergyPlus has five objects: steam boiler, steam pipe, steam to air coil, steam baseboard radiator, and condensate pump. The steam loop has several assumptions that help simplify loop complexity and increase usability.

Steam side of the loop operates on constant saturation pressure of steam
Water side (condensate) of the loop operates at atmospheric pressure
Steam loop is to operate at saturated conditions, no superheated condition
Steam loop is assumed to have no transportation losses by friction and heat transfer with surroundings so that it maintains the quality of steam throughout the system constant value of 0 or 1
Boiler operation is assumed to generate steam at quality equal to 1 every time and steam enters the coils at boiler outlet conditions
Steam coils are designed with steam traps, which only allow condensed steam to leave the coil; hence the steam always condenses and leaves the coil at quality of 0

These assumptions are applied to the new objects: LoadProfile:Plant in a steam loop, districtheatingsteam, and steam to water heat exchanger.

## Approach ##

1. The current LoadProfile:Plant calculates the outlet water temperature based on the inlet water temperature from the plant loop and user inputs for the scheduled plant load and the requested flow rate. 
In the new LoadProfile:Plant, three additional input fields were added: Plant Loop Fluid Type (Water or steam); Degree of SubCooling (optional input for steam loop); and Degree of Loop SubCooling (otional input for steam loop). The new LoadProfile:Plant in a steam loop calculates the outlet mass flow rate based on the scheduled plant load and user inputs of degree of subcooling, because the inlet steam temperature and the outlet steam temperature before the steam trap are fixed to saturation temperature according to the assumption.

2. The current DistrictHeating or DistrictCooling calculates the output capacity necessary from the inlet temperature to the setpoint temperature for that loop with the given mass flow rate in Watts. DistrictHeatingSteam calculates the output capacity necessary from the latent heat with the given saturation temperature.

3. 
Figure 1 describes the loop structure with steam to water heat exchanger.

![figure1](https://github.com/EnergyPlus/blob/AddThreeSteamModulesWithNTUMethod/design/FY2021/figure1.png)


## Testing/Validation/Data Sources ##

1. Standard unit tests were used to verify the new modules.
2. Test performance of the new modules with example test files.

## Input Output Reference Documentation ##


## Input Description ##

HeatExchanger:SteamToWater,
        \memo A steam to water heat exchanger designed to couple the steam loop to the water loop
   A1 , \field Name
        \required-field
        \reference-class-name validPlantEquipmentTypes
        \reference validPlantEquipmentNames
        \reference-class-name validCondenserEquipmentTypes
        \reference validCondenserEquipmentNames
        \reference-class-name validBranchEquipmentTypes
        \reference validBranchEquipmentNames
   A2 , \field Availability Schedule Name
        \note Availability schedule name for this system. Schedule value > 0 means the system is available.
        \note If this field is blank, the system is always available.
        \note default is that heat exchanger is on
        \type object-list
        \object-list ScheduleNames
   A3 , \field Steam Inlet Node Name
        \type node
        \required-field
   A4 , \field Steam Outlet Node Name
        \type node
        \required-field
   N1 , \field Steam Side Design Flow Rate
        \type real
        \required-field
        \minimum> 0.0
        \units m3/s
        \ip-units gal/min
        \autosizable
   A5 , \field Water Inlet Node Name
        \type node
        \required-field
        \note This connection is to the demand side of a loop and is the inlet to the heat exchanger
   A6 , \field Water Outlet Node Name
        \type node
        \required-field
        \note This connection is to the demand side of a loop
   N2 , \field Water Side Design Flow Rate
        \type real
        \required-field
        \minimum> 0.0
        \units m3/s
        \ip-units gal/min
        \autosizable
   N3 , \field Heat Exchanger U-Factor Times Area Value
        \type real
        \units W/K
        \minimum> 0.0
        \autosizable
        \required-field
   A7 , \field Control Type
        \type choice
        \key UncontrolledOn
        \key OperationSchemeModulated
        \key OperationSchemeOnOff
        \key HeatingSetpointModulated
        \key HeatingSetpointOnOff
        \default UncontrolledOn
   A8 , \field Heat Exchanger Setpoint Node Name
        \note Setpoint node is needed with any Control Type that is "*Setpoint*"
        \type node
   N4 , \field Minimum Temperature Difference to Activate Heat Exchanger
        \note Tolerance between control temperatures used to determine if heat exchanger should run.
        \type real
        \minimum 0.0
        \maximum 50
        \default 0.01
        \units deltaC
   A9, \field Heat Transfer Metering End Use Type
        \note This field controls end use reporting for heat transfer meters
        \type choice
        \key FreeCooling
        \key HeatRecovery
        \key HeatRejection
        \key HeatRecoveryForCooling
        \key HeatRecoveryForHeating
        \key LoopToLoop
        \default LoopToLoop
   N5 , \field Sizing Factor
        \note Multiplies the autosized flow rates for this device
        \type real
        \minimum> 0.0
        \default 1.0
   N6 , \field Operation Minimum Temperature Limit
        \note Lower limit on inlet temperatures, heat exchanger will not operate if either inlet is below this limit
        \type real
        \units C
   N7 ; \field Operation Maximum Temperature Limit
        \note Upper limit on inlet temperatures, heat exchanger will not operate if either inlet is above this limit
        \type real
        \units C

## Engineering Reference ##



## Example File and Transition Changes ##

No transition required. New example file(s) will be included.

## References ##


