Zone Air Mass Flow Balance Improvement
=====================================

**Bereket Nigusse, FSEC/UCF**

 - Original Date: January 19, 2021
 - Revision Date: none
 

## Justification for New Feature ##

ZoneAirMassFlowConservation is intended to balance airflows but current design is that the procedure adjusts zone mixing and/or infiltration rates. User that have complex air transfer models like grocery stores, restaurants, and some big box retailers need the ability to model actual air transfer and account for ventilation movement between spaces. In these cases, users want the infiltration, zone mixing, and exhaust fan flow rates to stay fixed at user specified flows, instead adjust the system return air to balance zone air flow. 

This new feature provides an alternative air flow balancing method that maintains the zone mixing and/or infiltration rates at user specified flow rate.

## E-mail and  Conference Call Conclusions ##

NA

## Overview ##

This work will implement code changes that allow alternative approach of the zone air mass flow balance calculation method that does not alter the simple flow objects mass flow. 
The fundamental zone mass balance equation remains the same but what this feature requesting is which zone air mass flow component needs to be adjusted to guarantee zone air mass flow conservation.

## Approach ##

Current zone air mass flow conservation equation is formulated to solve the following equation by adjusting the Zone Mixing flow, and zone infiltration flow sequentially.

 0.0 = [m_{sup} - m_{exh} - m_{ret} + m_{zmreceiving} - m_{zmsource} + m_{inf}]
	
where,
m_{sup} = Zone Supply Air Mass Flow Rate, [kg/s]
m_{exh} = Zone Exhaust Air Mass Flow Rate, [kg/s]
m_{ret} = Zone Return Air Mass Flow Rate, [kg/s]
m_{zmreceiving} = Zone Mixing Receiving Air Mass Flow Rate, [kg/s]
m_{zmsource} = Zone Mixing Source Air Mass Flow Rate, [kg/s]
m_{inf} = Zone Infiltration Air Mass Flow Rate, [kg/s]

The existing procedure first attempts to solve the above equation by adjusting the Zone Mixing flow only and if that does not satisfy the above equation, then the user specified zone infiltration air flow rate is adjusted as needed.  The zone infiltration air flow can be adjusted using two different methods depending user preference. The zone infiltration air flow adjusting method is a choice input field in the ZoneAirMassFlowConservation object.

The requested feature can be implemented by adjusting the return air mass flow but the return air mass flow rate can be varied between the minimum value of zero and a maximum value of the zone return air maximum flow rate. Since varying the return air mass flow only provides limiting flexibility in achieving balanced flow for the range of practical applications, the zone air flow mass balance requires two different solution scenarios: one when the zone is under positive pressure and when the zone is under negative pressure.

Solving the zone air flow mass balance equation for zone return air mass flow:

m_{ret} = [m_{sup} - m_{exh} + m_{zmreceiving} - m_{zmsource} + m_{inf}]
 
Depending on the sign of the value of return air mass flow rate calculated using the above equation two different cases that require different solution scheme are formulated:

Case I:  [m_{ret} >= 0.0]

         The zone is under positive pressure and requires return air mass flow rate to balance the zone air flow. The return air flow rate will be adjusted within the bounds of 0 and maximum return air flow rate to balance the zone air mass flow.

Case II: [m_{ret} < 0.0]
            
         The zone is under negative pressure and requires additional supply air flow to balance the zone air flow. In this case the return air mass flow rate will be reset to zero and the supply air flow rate is increased proportionally.
		 [m_{ret} = 0.0;]
		 [m_{sup} += m_{ret};]
		 

In both cases the zone mixing and infiltration airflow objects flow are maintained at user specified values. 


## Testing/Validation/Data Sources ##

(1) Any new subroutine(s) will have a unit test that validate that the subroutine is functioning properly.  
(2) Verifies the zone air flow mass balance without altering user specified Zone Mixing flows.

## Input Output Reference Documentation ##

This new feature will be implemented by adding a new choice key "AdjustReturnAirFlow” to the existing input field "Adjust Zone Mixing For Zone Air Mass Flow Balance" in the ZoneAirMassFlowConservation object. 

The existing input field "Adjust Zone Mixing For Zone Air Mass Flow Balance" better be renamed to "Zone Air Mass Flow Balance Method" and the two existing choice keys needs to be renamed as well for clarity. For example the "Yes" choice key will be replaced with "AdjsutZoneMixingFlow" and the "No" choice key with "None".

## Input Description ##

See modified sample "ZoneAirMassFlowConservation" object:


ZoneAirMassFlowConservation,
       \memo Enforces the zone air mass flow balance by adjusting zone mixing object and/or
       \memo infiltration object mass flow rates.
       \memo If either mixing or infiltration is active, then the zone air mass flow
       \memo balance calculation will attempt to enforce conservation of mass for each zone.
       \memo If mixing is "None" and infiltration is "None", then the zone air mass flow
       \memo calculation defaults to assume self-balanced simple flow mixing and infiltration objects.
       \unique-object
       \min-fields 3
  A1,  \field Zone Air Mass Flow Balance Method
       \note If AdjsutZoneMixingFlow, Zone mixing object flow rates are adjusted to balance the zone air mass flow
       \note and additional infiltration air flow may be added if required in order to balance the
       \note zone air mass flow.
       \type choice
       \key AdjsutZoneMixingFlow
       \key None

New choice Key:
       \key AdjustZoneReturnFlow
	   \note The zone air return air mass flow is adjusted to balance the zone air mass flow and 
	   \note if that does not guarantee the zone air mass flow conservation, then adjusts zone  
	   \note infiltration air flows per the user specified methods in the input field below.
	   \note Zone Mixing objects mass flow is maintained at user specified value.
	   
       \default No
  A2,  \field Infiltration Balancing Method
       \note This input field allows user to choose how zone infiltration flow is treated during
       \note the zone air mass flow balance calculation.
       \type choice
       \key AddInfiltrationFlow
       \key AdjustInfiltrationFlow
       \key None
       \default AddInfiltrationFlow
       \note AddInfiltrationFlow may add infiltration to the base flow specified in the
       \note infiltration object to balance the zone air mass flow. The additional infiltration
       \note air mass flow is not self-balanced. The base flow is assumed to be self-balanced.
       \note AdjustInfiltrationFlow may adjust the base flow calculated using
       \note the base flow specified in the infiltration object to balance the zone air mass flow. If it
       \note If no adjustment is required, then the base infiltration is assumed to be self-balanced.
       \note None will make no changes to the base infiltration flow.
  A3;  \field Infiltration Balancing Zones
       \note This input field allows user to choose which zones are included in infiltration balancing.
       \note MixingSourceZonesOnly allows infiltration balancing only in zones which as source zones for mixing
       \note which also have an infiltration object defined.
       \note AllZones allows infiltration balancing in any zone which has an infiltration object defined.
       \type choice
       \key MixingSourceZonesOnly
       \key AllZones
       \default MixingSourceZonesOnly

## Outputs Description ##

No new output variables will be implemented.

## Engineering Reference ##

Will be updated as needed. 

## Example File and Transition Changes ##

One new example file will be added to the test suite to demonstrate this feature is functioning properly. IDD change is required to add new choice key to an existing input field. Transition changes may be required if we choose to rename an existing input fields and the two key choices. 

## References ##

NA
