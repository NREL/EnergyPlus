Allow Multiple Air Primary Loops in the Airflow Network Model
================

**Lixing Gu**

**Florida Solar Energy Center**

 - Original: 1/26/18
 - 
 

## Justification for New Feature ##

The existing Airflow Network model only allows one air primary loop to calculate the energy losses for an air distribution system. This restriction makes the model only able to simulate energy performance with air distribution system losses in residential homes and small commercial buildings. Large commercial buildings are not covered by the model, because multiple air distribution systems, equivalent to multiple air primary loops in EnergyPlus, are expected. In order to make the Airflow Network model applicable to large building simulations, multiple air primary loops must be allowed.

## E-mail and  Conference Call Conclusions ##

### Chicago meeting 

Jason and Lixing were met in Chicago to discuss the first draft on 1/23/18.

A new filed of AirLoop Name in AirflowNetwork:Distribution:Linkage was proposed in the first draft to tell the program that this linkage is a part of this specific AirLoop. The review comment from Jason DeGraw was that this new field is not necessary, because the AirLoop information is available. The linkage check can be accomplished internally. Therefore, the new field will not be proposed.

Jason also mentioned to allow an OA mixer for each AirLoop. Since multiple AirLoops are allowed, multiple OA mixers will be allowed too. A new field of OA mixer name will be inserted to accommodate multiple OA mixers in AirflowNetwork:Distribution:Component:OutdoorAirFlow and AirflowNetwork:Distribution:Component:ReliefAirFlow. 

## Overview ##

The Airflow Network model will be modified to permit the use of a configurable number of air distribution systems. The proposed upgrade will simulate energy losses for air distribution systems and associated interactions among HVAC systems, envelope, and outdoors caused by unbalanced return and supply leaks in the multiple air distribution systems in a building.

## Approach ##

The following revisions will be performed:

### Revise a filed name in AirflowNetwork:Distribution:Component:OutdoorAirFlow

The
 
### GetAirflowNetworkInput

A few sections in this function will be modified to accommodate the changes. 

#### Remove a section to check number of air loops

There is a section to check the number of primary air loops to ensure a single air loop will be used in the Airflow Network model. The restriction will be removed to allow multiple air loop in the model.

#### Check OutdoorAir:Mixer object

When the field of OA mixer name in AirflowNetwork:Distribution:Component:OutdoorAirFlow AirflowNetwork:Distribution:Component:ReliefAirFlow is added, the reading section will check the object name to ensure the the OA mixer name is valid. 

#### Add a filed of OA mixer name in AirflowNetwork:Distribution:Component:OutdoorAirFlow

Since this new feature will allow multiple AirLoops, multiple OutdoorAir:Mixer objects will be allowed. The restriction is that each AirLoop has a single OutdoorAir:Mixer. The new field of OA mixer Name is to specify this object is a part of a corresponding OA mixer or an AirLoop.  

	The name identifying an AirflowNetwork OutdoorAirFlow defined in an air loop as a part of OutdoorAir:Mixer object. This name must be the same name as the associated OutdoorAir:Mixer object. 

It should be pointed out that an OA mixer has 4 nodes. This component represents an outdoor air pathway.

#### Add a filed of OA mixer name in AirflowNetwork:Distribution:Component:ReliefAirFlow

Since this new feature will allow multiple AirLoops, multiple OutdoorAir:Mixer objects will be allowed. The restriction is that each AirLoop has a single OutdoorAir:Mixer. The new field of OA mixer Name is to specify this object is a part of a corresponding OA mixer or an AirLoop.  

	The name identifying an AirflowNetwork OutdoorAirFlow defined in an air loop as a part of OutdoorAir:Mixer object. This name must be the same name as the associated OutdoorAir:Mixer object. 

It should be pointed out that an OA mixer has 4 nodes. This component represents a relief air pathway.

### ValidateDistributionSystem

This function validates the inputs of distribution system to ensure all nodes served by an AirLoop are defined in both objects of AirflowNetwork:Distribution:Linkage and AirflowNetwork:Distribution:Node. Since this new feature allows multiple air loops in the Airflow Network model, this function will be modified to validate the inputs of distribution system for each air loop.

All the nodes defined in all air loops have to be defined in AirflowNetwork:Distribution:Node objects and used in AirflowNetwork:Distribution:Linkage objects. In addition, the inputs will be validated to ensure nodes defined in a linkage should belong to the same air loop.  


## Testing/Validation/Data Sources ##

A test file with multiple airloops and AirflowNetowrk model will be created to ensure the AirflowNetowkr model with multiple airloops will work properly. 

## Input Output Reference Documentation ##

Two objects will be modified to specify that the objects are served by a corresponding AirLoop or outdoor air mixer. 

### AirflowNetwork:Distribution:Component:OutdoorAirFlow

\subsection{AirflowNetwork:Distribution:Component:OutdoorAirFlow}\label{airflowNetworkdistributioncomponentoutdoorairflow}

The AirflowNetwork:Distribution:Component:OutdoorAirFlow object is used to allow the AirflowNetwork model to include the outdoor air flow rate in the airflow network. When the outdoor air mass flow rate is greater than zero, the airflow network model treats this object as a constant volume fan and the flow rate is provided by the Controller:OutdoorAir object. When there is no outdoor air flow rate, the model treats this object as a crack and a power law is assumed.

\subsubsection{Inputs}\label{inputs-AFN-outdoorairflow}

\paragraph{Field: Name}\label{field-name-outdoorairflow}

This is the name for this instance of the AirflowNetwork:Distribution:Component:OutdoorAirFlow object. This unique name will be referenced by an AirflowNetwork:Distribution:Linkage object to represent a component.

<span style="color:red;">**\paragraph{Field: Outdoor Air Mixer Name}\label{field-name-outdoor-air-mixer}**

<span style="color:red;">The name identifying an AirflowNetwork OutdoorAirFlow defined in an air loop as an OutdoorAir:Mixer object. This name must be the same name as the associated OutdoorAir:Mixer object.    

\paragraph{Field: Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions}\label{field-air-mass-flow-coefficient-when-no-outdoor-air-flow-at-reference-conditions}

The value of the air mass flow coefficient, \({C_Q}\), in the crack air flow equation. It has units of kg/s at 1Pa. This value must be greater than zero. The value is used when when the outdoor mass flow rate is zero from the Controller:OutdoorAir object.

\paragraph{Field: Air Mass Flow Exponent When No Outdoor Air Flow}\label{field-air-mass-flow-exponent-when-no-outdoor-air-flow}

The value of the exponent, \emph{n}, in the crack air flow equation. The valid range is 0.5 to 1.0, with the default value being 0.65. The value is used when the outdoor mass flow rate is zero from the Controller:OutdoorAir object.

\paragraph{Field: Reference Crack Conditions}\label{field-reference-crack-conditions-2016-06-16-1612}

The name of the AirflowNetwork:MultiZone:ReferenceCrackConditions object which specifies the conditions under which the air mass flow coefficient was measured. If the user omits this field and only one AirflowNetwork:MultiZone:ReferenceCrackConditions object is defined in the input data file, then those reference crack conditions will be used. If the user omits this field and either zero or more than one AirflowNetwork:MultiZone:ReferenceCrackConditions objects are defined in the input data file, then the default conditions for the AirflowNetwork:MultiZone: Reference Crack Conditions object will be used.

IDF examples are provided below:

\begin{lstlisting}

AirflowNetwork:MultiZone:Component:OutdoorAirFlow,
  OAFlow,                  !- Name
  OAMixer,                 !- Outdoor Air Mixer Name
  0.01,                    !- Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions {kg/s}
  0.667;                   !- Air Mass Flow Exponent When No Outdoor Air Flow {dimensionless}

### AirflowNetwork:Distribution:Component:ReliefAirFlow

\subsection{AirflowNetwork:Distribution:Component:ReliefAirFlow}\label{airflowNetworkdistributioncomponentreliefairflow}

The AirflowNetwork:Distribution:Component:ReliefAirFlow object is used to allow the AirflowNetwork model to perform pressure control by varying the amount of relief air flow rate between 0 and the flow rate specified by the Controller:OutdoorAir object. When the outdoor air mass flow rate is greater than zero, the airflow network model treats this object as a constant volume fan and the flow rate is varied to reach pressure control. When there is no outdoor air flow rate, the model treats this object as a crack and a power law is assumed.

\subsubsection{Inputs}\label{inputs-AFN-reliefairflow}

\paragraph{Field: Name}\label{field-name-reliefairflow}

This is the name for this instance of the AirflowNetwork:Distribution:Component:ReliefAirFlow object. This unique name will be referenced by an AirflowNetwork:Distribution:Linkage object to represent a component.

<span style="color:red;">**\paragraph{Field: Outdoor Air Mixer Name}\label{field-name-outdoor-air-mixer}**

<span style="color:red;">The name identifying an AirflowNetwork OutdoorAirFlow defined in an air loop as an OutdoorAir:Mixer object. This name must be the same name as the associated OutdoorAir:Mixer object.    

\paragraph{Field: Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions}\label{field-air-mass-flow-coefficient-when-no-outdoor-air-flow-at-reference-conditions-2016-06-16}

The value of the air mass flow coefficient, \({C_Q}\), in the crack air flow equation. It has units of kg/s at 1Pa. This value must be greater than zero. The value is used when the outdoor mass flow rate is zero from the Controller:OutdoorAir object.

\paragraph{Field: Air Mass Flow Exponent When No Outdoor Air Flow}\label{field-air-mass-flow-exponent-when-no-outdoor-air-flow-2016-06-16}

The value of the exponent, \emph{n}, in the crack air flow equation. The valid range is 0.5 to 1.0, with the default value being 0.65. The value is used when the outdoor mass flow rate is zero from the Controller:OutdoorAir object.

\paragraph{Field: Reference Crack Conditions}\label{field-reference-crack-conditions-2016-06-16-1613}

The name of the AirflowNetwork:MultiZone:ReferenceCrackConditions object which specifies the conditions under which the air mass flow coefficient was measured. If the user omits this field and only one AirflowNetwork:MultiZone:ReferenceCrackConditions object is defined in the input data file, then those reference crack conditions will be used. If the user omits this field and either zero or more than one AirflowNetwork:MultiZone:ReferenceCrackConditions objects are defined in the input data file, then the default conditions for the AirflowNetwork:MultiZone: Reference Crack Conditions object will be used.

## Input Description ##

### AirflowNetwork:Distribution:Component:OutdoorAirFlow

A new field will be inserted in the AirflowNetwork:Distribution:Component:OutdoorAirFlow object, so that transition may be needed. Any new additions will be highlighted in red.

	AirflowNetwork:Distribution:Component:OutdoorAirFlow,
      \min-fields 3
      \memo This object includes the outdoor air flow rate set by the Controller:OutdoorAir
      \memo object in the airflow network.
      \unique-object
 	A1 , \field Name
      \required-field
      \reference AFNOutdoorAirFlowNames
 <span style="color:red;">**	A2 , \field Outdoor Air Mixer Name **

      \required-field
      \object-list OutdoorAirMixers
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
      \note Used only when no outdoor air flow rate.
 	N2 , \field Air Mass Flow Exponent When No Outdoor Air Flow
      \units dimensionless
      \type real
      \minimum 0.5
      \maximum 1.0
      \default 0.65
      \note Enter the exponent used in the following equation:
      \note Mass Flow Rate = Air Mass Flow Coefficient * (dP)^Air Mass Flow Exponent.
      \note Used only when no outdoor air flow rate.
 	A3 ; \field Reference Crack Conditions
      \type object-list
      \object-list ReferenceCrackConditions
      \note Select a AirflowNetwork:MultiZone:ReferenceCrackConditions name associated with
      \note the air mass flow coefficient entered above.

### AirflowNetwork:Distribution:Component:ReliefAirFlow

A new field will be inserted in the AirflowNetwork:Distribution:Component:ReliefAirFlow object, so that transition may be needed. Any new additions will be highlighted in red.

	AirflowNetwork:Distribution:Component:ReliefAirFlow,
      \min-fields 3
      \memo This object allows variation of air flow rate to perform pressure.
      \unique-object
 	A1 , \field Name
      \required-field
      \reference AFNReliefAirFlowNames
 <span style="color:red;">**	A2 , \field Outdoor Air Mixer Name **

      \required-field
      \object-list OutdoorAirMixers
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
      \note Used only when no outdoor air flow rate.
 	N2 , \field Air Mass Flow Exponent When No Outdoor Air Flow
      \units dimensionless
      \type real
      \minimum 0.5
      \maximum 1.0
      \default 0.65
      \note Enter the exponent used in the following equation:
      \note Mass Flow Rate = Air Mass Flow Coefficient * (dP)^Air Mass Flow Exponent.
      \note Used only when no outdoor air flow rate.
 	A3 ; \field Reference Crack Conditions
      \type object-list
      \object-list ReferenceCrackConditions
      \note Select a AirflowNetwork:MultiZone:ReferenceCrackConditions name associated with
      \note the air mass flow coefficient entered above.


## Outputs Description ##

insert text

## Engineering Reference ##

insert text

## Example File and Transition Changes ##

A new field name will be inserted in both objects:  AirflowNetwork:Distribution:Component:OutdoorAirFlow and AirflowNetwork:Distribution:Component:ReliefAirFlow.

## References ##

https://energyplus.uservoice.com/forums/258860-energyplus/suggestions/20318116-allow-multiple-air-loops-in-the-airflow-network-mo

HVAC_AirLoop_2009_06.doc: Allow Multiple Air Primary Loops in the AirflowNetwork Model



