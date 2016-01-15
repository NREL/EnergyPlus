Pressure Control Through Pressure Controller
================

**Lixing Gu** 

**Florida Solar Energy Center**

**Third revision**
1/12/16
Revise NFP based on comments from the first review meeting.


**Second revision**
1/8/16
Allow pressure control by adjusting exhaust fan flow rate, as well as relief node flow rate in an OA mixer

**First revision**
1/6/16
Added a new object of AirflowNetwork:Distribution:Component:OutdoorAirFlow to handle OA flow rate

**Initial draft**
 - 12/23/15
 

## Justification for New Feature ##

The Airflow Network model currently implemented in EnergyPlus is used to accurately predict simulation conditions based on a pressure-network. This capability allows users to carefully define the pressure components in their building, and use this to resolve flow rates and infiltration rates.

One piece missing from the Airflow Network capabilities is the ability to control to a pressure setpoint within a space. The pressure control is able to let designers to control unintentional airflows through the building envelope and between spaces inside a building to prevent a number of adverse impacts of these airflows. In addition, pressure control is required for some special spaces, such as operation room and chemical lab. The new feature proposal adds this specific capability to the existing Airflow Network model in EnergyPlus.


## E-mail and Conference Call Conclusions ##

###E-mail communications

Tianzhen comments from E-mail on 12/23/15

Three quick questions:

1. Does it require or allow each zone to have its own exhaust fan? its own pressure setpoint? Or this only applies to a control zone for an air loop? I saw more common for office buildings is to use a central relief fan to control air pressure in the building.

2. Would the naming ZoneControl:PressureStat make more sense? As this only applies to AFN.

3. For leaky buildings, sometimes the only way to control pressure is to increase OA flow rate. 

Gu's reply on 12/28/15

1A: The proposal proposes an exhaust fan in a controlled zone for pressure control in an AirLoop. It is similar to temperature control to use a thermostat in a controlled zone. The pressure control only happens in the controlled zone. 

It is true that a central relief fan is also used to control building pressure. The existing EnergyPlus does not have an object for a central relief fan. In turn, the AirflowNetwork model does not also have the same object. That is why an exhaust fan is proposed.   

2A: The new object is applied to AFN only as proposed. I don’t have any strong opinions for the name of a new object. My intent is that the object may not be restricted to AFN. It is possible that E+ may also calculate zone pressure in other models in the future.  We can discuss it.

3A: It is also true to adjust OA for zone pressure control. If we allow to adjust both OA and exhaust flow rates, there is no unique solution. Therefore, exhaust flow rate is proposed. If pressure control can not be reached in leaky buildings, users can increase OA. I will add it in a warning message.  

A new addition on 1/8/16

A central relief control is added. It addresses one of Tianzhen's concerns.

###Review meeting

The first review meeting was held on 1/12/16. Michael Witte, Jason DeGraw, Tianzhen Hong and Lixing Gu attended the meeting.

The main concern is the object name of the PressureStat. The consensus conclusion is a new object name: AirflowNetwork:ZoneControl:PressureController to replace ZoneControl:PressureStat.

Another concern is to prevent circulation of OA flow and zone exhaust fan flow rate. The current practice is that when exhaust fan flow rate is greater than OA flow rate, the OA flow rate will increase to match exhaust fan flow rate. This concern will be considered during coding to avoid possible circulation of both airflow rates.  

## Overview ##

The pressure control could be achieved by changing either exhaust fan flow rate in a controlled zone or relief node flow rate in an OA mixer, when an AirLoop has two required components: an outdoor air mixer and an exhaust fan in a controlled zone. Figure 1 shows schematic of an AirLoop with required components. The proposed pressure control will be accomplished in the AirLoop. 

The outdoor flow rate is specified by the Controller:OutdoorAir object in the AirLoop. The first approach is to vary exhaust fan flow rate between zero and maximum flow rates to achieve pressure control in the controlled zone. The maximum flow rate of an exhaust fan is specified in the Fan:ZoneExhaust object. The relief node flow rate is zero. The second approach is to vary relief node flow rate in an OA mixer between zero flow rate and the OA flow rate specified by the Controller:OutdoorAir object. The exhaust fan flow rate will be fixed.   


![](PressurestatFigure1.png)

Figure 1.  Simplified Schematic of an Airloop with an OA mixer and an exhaust fan in a zone


## Approach ##

The proposed approach adds three new objects and enhances outdoor air, relief air and exhaust fan handling in the AirflowNetwork model. The first new object is to provide pressure setpoint. The second one is to allow the AirflowNetwork model to adopt the outdoor air flow rate based on the Controller:OutdoorAir object. The third one is to set up adjustable relief air flow rate to meet the zone pressure setpoint.

###A new object of PressureController

A new object of AirflowNetwork:ZoneControl:PressureController will be proposed to allow a user to input pressure setpoint and a controlling component with adjustable airflow rate to meet the setpoint. The proposed component is either the Fan:ZoneExhaust object or the OutdoorAir:Mixer. A single choice is allowed. In other words, both Fan:ZoneExhaust and OutdoorAir:Mixer objects can not used together to perform pressure control.

Since the Fan:ZoneExhaust has the maximum flow rate field, there is no need to revise the existing object. 

Since the relief node flow rate change is based on the outdoor airflow rate specified by the Controller:OutdoorAir object, there is no need to have flow rate inputs.

###A new object of AirflowNetwork:Distribution:Component:OutdoorAirFlow

A new object of AirflowNetwork:Distribution:Component:OutdoorAirFlow will be proposed to allow a user to adopt the outdoor air flow rate based on the Controller:OutdoorAir object. When the outdoor air flow rate is zero, the model treats this component as a crack using a power law to specify relationship between pressure difference and mass flow rate.

It should be pointed out that the object does not have information on Controller:OutdoorAir object, because the AirflowNetwork model allows a single Controller:OutdoorAir object based on restriction of a single AirLoop. When multiple Airloops are allowed, the required optional inputs of Controller:OutdoorAir objects will be added.  

###A new object of AirflowNetwork:Distribution:Component:ReliefAirFlow

A new object of AirflowNetwork:Distribution:Component:ReliefAirFlow will be proposed to allow a user to set up the relief node flow in an OA mixer to perform pressure control. When the outdoor air flow rate is zero, the model treats this component as a crack using a power law to specify relationship between pressure difference and mass flow rate.

It should be pointed out that the object does not have information on the OA mixer object. The choice is provided in the PressureController object. 

### Outdoor air handling

There are two ways to treat an OA mixer in the existing AirflowNetwork model. The first way is that there is no link between an outdoor air node and an OA mixer, so that returning airflow rate is equal to the one at OA mixer outlet node. This is the same way to treat airflow rate for an OA mixer without using the AirflowNetwork model. The mixer outlet node conditions are calculated using the current code in MixedAir module. The advantage is that no mass flow rate adjustment is needed and the mixer outlet calculation results are applied to the mixer outlet. The disadvantage is the return flow rate is not treated in reality.   

The second way is to add a link between the outdoor air node and the OA mixer. However, the outdoor air flow rate is calculated based on linkage resistance and pressure difference across the link. In other words, the OA flow rate is not controlled precisely compared to the OA controller specification, and varies with time, since outdoor pressure changes with time.

The proposed approach is to have a link between the outdoor air node and the OA mixer and an associated new flow component. The new component will be a constant fan when the outdoor air flow from the Controller:OutdoorAir object is greater than zero, and a crack with zero outdoor flow rate.

In order to use a central relief airflow to preform pressure control, an additional link between the OA mixer and the outdoor air node is needed. The link is similar to the above link. The difference is that the orders of two distribution nodes are switched. The above link represent an OA fan, while the current link represents a central relief fan.      

### Exhaust fan handling

Since the new feature requires to vary exhaust fan flow rate to achieve the pressure control, following procedures will be performed:

1. The AirflowNetwork model will run twice with zero and maximum flow rates for the controlled zone exhaust fan. The controlled zone pressures are the return values.

	In general, the zero flow rate of either exhaust fan or relief node will generate the maximum zone pressure, while the maximum flow rate of either exhaust fan or relief nodewill produce the minimum zone pressure. 

2. If the setpoint pressure is between the maximum zone pressure caused by zero exhaust fan flow rate and the minimum zone pressure caused by maximum exhaust fan flow rate, The AirflowNetwork model will use Regula Falsi to find a solution with calculated zone exhaust fan flow rate. Otherwise, the pressure setpoint will not be met. 

3. If both maximum and minimum pressures are higher than the setpoint, the maximum exhaust flow rate will be forced. 

4. If both maximum and minimum pressures are lower than the setpoint, the zero exhaust flow rate will be forced.

### Relief air handling

The relief air handling is similar to exhaust air handling. The logic is the same. The difference is that relief air handling is based on a system, while exhaust air handling occurs in a controlled zone. 

## Testing/Validation/Data Sources ##

The simulation results will be compared to spread sheet results.

## Input Output Reference Documentation ##

This section describes inputs of three new object as AirflowNetwork:ZoneControl:PressureController, AirflowNetwork:Distribution:Component:OutdoorAirFlow and AirflowNetwork:Distribution:Component:ReliefAirFlow. 

### AirflowNetwork:ZoneControl:PressureController

The AirflowNetwork:ZoneControl:PressureController object is used to control a zone to a specified indoor level of pressure using the AirflowNetwork model. The specified pressure setpoint is used to calculate the required zone exhaust fan flow rate in a controlled zone or relief air flow rate in an AirLoop.

The object has the same performance as ZoneControl:Thermostat. When an AirLoop serves multiple zones, the controlled zone will reach the specific setpoint, while other zones will not be controlled precisely.

#### Field: Name

Unique identifying name for the AirflowNetwork:Distribution:Component:OutdoorAirFlow.

#### Field: Controlled Zone Name

Name of the zone that is being controlled.

#### Field: Control Object type

This field specifies the control type to be used for pressure control. Available control types are:
Fan:ZoneExhaust and OutdoorAir:Mixer.

#### Field: Control Name

The corresponding control type name. 

#### Field:Pressure Control Availability Schedule Name

This field contains the name of a schedule that determines whether or not the AirflowNetwork:ZoneControl:PressureController is available. When the schedule value is zero, the AirflowNetwork:ZoneControl:PressureController is bypassed (not available to operate). When the schedule value is greater than zero, the AirflowNetwork:ZoneControl:PressureController is available and will be used to calculate the required zone exhaust fan airflow rate to reach the pressure setpoint when an exhaust fan is used to preform pressure control. When an OutdoorAir:Mixer is entered, the required airflow is the central relief flow rate. If this field is left blank, the schedule has a value of 1 for all time periods. Schedule values must be between 0 and 1.

#### Field:Pressure Setpoint Schedule Name

This field contains the name of a schedule that contains the zone air pressure setpoint as a function of time. The units for pressure setpoint are Pascal. The setpoint values in the schedule must be between -50 and 100 Pascal. 

An IDF example is provided below:

```idf
   AirflowNetwork:ZoneControl:PressureController,
       Pressure Controller1,           !- Name
       EAST ZONE,                      !- Controlled Zone Name
	   Fan:ZoneExhaust,                !- Control Object type
	   East Zone Exhaust Fan,          !- Control Name
       PressureAvailSchedule,          !- Pressure Control Availability Schedule Name
       PressureSetpointSchedule;       !- Pressure Setpoint Schedule Name
```

###AirflowNetwork:Distribution:Component:OutdoorAirFlow

The AirflowNetwork:Distribution:Component:OutdoorAirFlow object is used to allow the AirflowNetwork model to adopt the amount of outdoor air flow rate. When the outdoor air mass flow rate is greater than zero, the airflow network model treats this object as a constant volume fan and the flow rate is provided by the Controller:OutdoorAir object. When there is not outdoor air flow rate, the model treats this object as a crack and a power law is assumed. 

####Field: Name

This is the name for this instance of the AirflowNetwork:Distribution:Component:OutdoorAirFlow object.

####Field: Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions

The value of the air mass flow coefficient,({C_Q}), in the crack air flow equation. It has units of kg/s at 1Pa. This value must be greater than zero. The value is used when the outdoor mass flow rate is zero from the Controller:OutdoorAir object.

####Field: Air Mass Flow Exponent When No Outdoor Air Flow

The value of the exponent,* n*, in the crack air flow equation. The valid range is 0.5 to 1.0, with the default value being 0.65. The value is used when the fan is off. The value is used when the outdoor mass flow rate is zero from the Controller:OutdoorAir object.

####Field: Reference Crack Conditions

The name of the AirflowNetwork:MultiZone:ReferenceCrackConditions object which specifies the conditions under which the air mass flow coefficient was measured. If the user omits this field and only one AirflowNetwork:MultiZone:ReferenceCrackConditions object is defined in the input data file, then those reference crack conditions will be used. If the user omits this field and either zero or more than one AirflowNetwork:MultiZone:ReferenceCrackConditions objects are defined in the input data file, then the default conditions for the AirflowNetwork:Multizone: Reference Crack Conditions object will be used.

IDF examples are provided below:

```idf

  AirflowNetwork:MultiZone:Component:OutdoorAirFlow,
    OAFlow,                  !- Name
    0.01,                    !- Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions {kg/s}
    0.667;                   !- Air Mass Flow Exponent When No Outdoor Air Flow {dimensionless}

  AirflowNetwork:Distribution:Node,
    OA System Node,          !- Name
    ,                        !- Component Name or Node Name
    AirLoopHVAC:OutdoorAirSystem,  !- Component Object Type or Node Type
    3.0;                     !- Node Height {m}

  AirflowNetwork:Distribution:Node,
    OA Inlet Node,           !- Name
    Outside Air Inlet Node,  !- Component Name or Node Name
    OAMixerOutdoorAirStreamNode,  !- Component Object Type or Node Type
    1.5;                     !- Node Height {m}

  AirflowNetwork:Distribution:Linkage,
    OASystemFanLink,       !- Name
    OA Inlet Node,           !- Node 1 Name
    OA System Node,          !- Node 2 Name
    OAFlow;      !- Component Name

```
###AirflowNetwork:Distribution:Component:ReliefAirFlow

The AirflowNetwork:Distribution:Component:ReliefAirFlow object is used to allow the AirflowNetwork model to perform pressure control by varying the amount of relief air flow rate. When the outdoor air mass flow rate is greater than zero, the airflow network model treats this object as a constant volume fan and the flow rate is varied to reach pressure control. When there is not outdoor air flow rate, the model treats this object as a crack and a power law is assumed. 

####Field: Name

This is the name for this instance of the AirflowNetwork:Distribution:Component:ReliefAirFlow object.

####Field: Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions

The value of the air mass flow coefficient,({C_Q}), in the crack air flow equation. It has units of kg/s at 1Pa. This value must be greater than zero. The value is used when the outdoor mass flow rate is zero from the Controller:OutdoorAir object.

####Field: Air Mass Flow Exponent When No Outdoor Air Flow

The value of the exponent,* n*, in the crack air flow equation. The valid range is 0.5 to 1.0, with the default value being 0.65. The value is used when the fan is off. The value is used when the outdoor mass flow rate is zero from the Controller:OutdoorAir object.

####Field: Reference Crack Conditions

The name of the AirflowNetwork:MultiZone:ReferenceCrackConditions object which specifies the conditions under which the air mass flow coefficient was measured. If the user omits this field and only one AirflowNetwork:MultiZone:ReferenceCrackConditions object is defined in the input data file, then those reference crack conditions will be used. If the user omits this field and either zero or more than one AirflowNetwork:MultiZone:ReferenceCrackConditions objects are defined in the input data file, then the default conditions for the AirflowNetwork:Multizone: Reference Crack Conditions object will be used.

IDF examples are provided below:

```idf
   AirflowNetwork:ZoneControl:PressureController,
     Pressure Controller1,           !- Name
     EAST ZONE,                      !- Controlled Zone Name
	 OutdoorAir:Mixer,               !- Control Object type
	 OA Mixing Box 1,                !- Control Name
     PressureAvailSchedule,          !- Pressure Control Availability Schedule Name
     PressureSetpointSchedule;       !- Pressure Setpoint Schedule Name

   AirflowNetwork:MultiZone:Component:ReliefAirFlow,
     ReliefFlow,                     !- Name
     0.01,                           !- Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions {kg/s}
     0.667;                          !- Air Mass Flow Exponent When No Outdoor Air Flow {dimensionless}

  AirflowNetwork:Distribution:Node,
    OA System Node,                  !- Name
    ,                                !- Component Name or Node Name
    AirLoopHVAC:OutdoorAirSystem,    !- Component Object Type or Node Type
    3.0;                             !- Node Height {m}

  AirflowNetwork:Distribution:Node,
    OA Inlet Node,                   !- Name
    Outside Air Inlet Node,          !- Component Name or Node Name
    OAMixerOutdoorAirStreamNode,     !- Component Object Type or Node Type
    1.5;                             !- Node Height {m}

  AirflowNetwork:Distribution:Linkage,
    OASystemFanLink,                !- Name
    OA System Node,                 !- Node 1 Name
    OA Inlet Node,                  !- Node 2 Name
    ReliefFlow;                     !- Component Name

```

## Input Description ##

This section describes inputs of three new object as AirflowNetwork:ZoneControl:PressureController,  AirflowNetwork:Distribution:Component:OutdoorAirFlow, and AirflowNetwork:Distribution:Component:ReliefAirFlow. 

### New objects

AirflowNetwork:ZoneControl:PressureController

	AirflowNetwork:ZoneControl:PressureController,
   		\memo Define the Pressures control settings for a zone or list of zones.
   		\memo If you use a ZoneList in the Zone or ZoneList name field then this definition applies
   		\memo to all the zones in the ZoneList.
  	A1 , \field Name
       \required-field
       \reference AirflowNetworkZoneControlPressureControllerNames
 	A2 , \field Zone Name
       \required-field
       \type object-list
       \object-list ZoneAndZoneListNames
  	A3 , \field Control Object Type
       \required-field
       \type choice
       \key Fan:ZoneExhaust
	   \note The current selection is Fan:ZoneExhaust only.
  	A4 , \field Control Name
       \note Control names are names of individual control objects 
       \required-field
       \type object-list
       \object-list ControlTypeNames
  	A5 , \field Pressure Control Availability Schedule Name
       \note Availability schedule name for pressure controller. Schedule value > 0 means the 
	   \note pressure controller is enabled. If this field is blank, then pressure controller is \note always enabled.
       \type object-list
       \object-list ScheduleNames
  	A6 ; \field Pressure Setpoint Schedule Name
      \type object-list
      \object-list ScheduleNames


AirflowNetwork:Distribution:Component:OutdoorAirFlow

	AirflowNetwork:Distribution:Component:OutdoorAirFlow
      \min-fields 3
      \memo This object adopts outdoor air flow based on Controller:OutdoorAir object
 	A1 , \field Name
       \required-field
       \reference AFNOutdoorAirFlowNames
 	N1 , \field Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions
      \required-field
      \type real
      \units kg/s
      \minimum> 0
      \note Enter the air mass flow coefficient at the conditions defined
      \note in the Reference Crack Conditions object.
      \note Defined at 1 Pa pressure difference. Enter the coefficient used in the following
      \note equation:
      \note Mass Flow Rate = Air Mass Flow Coefficient * (dP)^Air Mass Flow Exponent.
      \note Used only when no outdoor iar flow rate.
 	N2 , \field Air Mass Flow Exponent When No Outdoor Air Flow
      \units dimensionless
      \type real
      \minimum 0.5
      \maximum 1.0
      \default 0.65
      \note Enter the exponent used in the following equation:
      \note Mass Flow Rate = Air Mass Flow Coefficient * (dP)^Air Mass Flow Exponent.
      \note Used only when no outdoor iar flow rate.
 	A2 ; \field Reference Crack Conditions
      \type object-list
      \object-list ReferenceCrackConditions
      \note Select a AirflowNetwork:MultiZone:ReferenceCrackConditions name associated with
      \note the air mass flow coefficient entered above.

AirflowNetwork:Distribution:Component:ReliefAirFlow

	AirflowNetwork:Distribution:Component:ReliefAirFlow
      \min-fields 3
      \memo This object allows variation of air flow rate to perform pressure. 
 	A1 , \field Name
       \required-field
       \reference AFNReliefAirFlowNames
 	N1 , \field Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions
      \required-field
      \type real
      \units kg/s
      \minimum> 0
      \note Enter the air mass flow coefficient at the conditions defined
      \note in the Reference Crack Conditions object.
      \note Defined at 1 Pa pressure difference. Enter the coefficient used in the following
      \note equation:
      \note Mass Flow Rate = Air Mass Flow Coefficient * (dP)^Air Mass Flow Exponent.
      \note Used only when no outdoor iar flow rate.
 	N2 , \field Air Mass Flow Exponent When No Outdoor Air Flow
      \units dimensionless
      \type real
      \minimum 0.5
      \maximum 1.0
      \default 0.65
      \note Enter the exponent used in the following equation:
      \note Mass Flow Rate = Air Mass Flow Coefficient * (dP)^Air Mass Flow Exponent.
      \note Used only when no outdoor iar flow rate.
 	A2 ; \field Reference Crack Conditions
      \type object-list
      \object-list ReferenceCrackConditions
      \note Select a AirflowNetwork:MultiZone:ReferenceCrackConditions name associated with
      \note the air mass flow coefficient entered above.

## Outputs Description ##

This section presents outputs from a new object of AirflowNetwork:ZoneControl:PressureController.

### AirflowNetwork:ZoneControl:PressureController Outputs

The following output variables are available when the AirflowNetwork:ZoneControl:PressureController object is specified.

* HVAC,Average,Zone Air Pressure Setpoint [Pa]

* HVAC,Average,Zone Air Pressure Control Mass Flow Rate [kg/s]

####HVAC,Average,Zone Air Pressure Setpoint [Pa]

This is the current zone pressure setpoint in Pascal. If the pressure control is not available determined by the availability schedule value, then the value will be 0. This value is set at each zone timestep. Using the averaged value for longer reporting frequencies (hourly, for example) may not be meaningful in some applications.

####HVAC,Average,Zone Air Pressure Control Mass Flow Rate [kg/s]

This is the mass flow rate used to achieve the pressure control in kg/s. If the pressure control is not available, then the value will be 0. This value is set at each zone timestep. Using the averaged value for longer reporting frequencies (hourly, for example) may not be meaningful in some applications.

## Engineering Reference ##

A new section of Pressure Control will be added under the AirflowNetwork model.
 
###AirflowNetwork Model
...
####Pressure Control

The pressure control is achieved by varying either zone exhaust fan flow rate in a controlled zone or relief air flow rate in an AirLoop. It requires an AirLoop with an OA mixer and an exhaust fan in a controlled zone. The calculation logic is provide below:


1. The AirflowNetwork model will run twice with zero and maximum flow rates for either the controlled zone exhaust fan or relief air flow rate. The controlled zone pressures are the return values.

	In general, the zero flow rate of either exhaust fan or relief node will generate the maximum zone pressure, while the maximum flow rate of either exhaust fan or relief nodewill produce the minimum zone pressure. 

2. If the setpoint pressure is between the maximum zone pressure caused by zero exhaust fan or relief node flow rate and the minimum zone pressure caused by maximum exhaust fan flow rate or outdoor air flow rate, The AirflowNetwork model will use Regula Falsi to find a solution with calculated zone exhaust fan flow rate. Otherwise, the pressure setpoint will not be met. 

3. If both maximum and minimum pressures are higher than the setpoint, the maximum exhaust or outdoor flow rate will be forced. 

4. If both maximum and minimum pressures are lower than the setpoint, the zero flow rate will be forced.
  
## Example File and Transition Changes ##

A new example file will be created to demonstrate pressure control.

No transition is needed.


## References ##

insert text




