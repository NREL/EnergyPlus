OnOff Thermostat
================

**Lixing Gu**

**Florida Solar Energy Center**

 - Original
 - 10/20/17

 

## Justification for New Feature ##

In general, real cycling systems do not cycle several times in a short period. EnergyPlys simulates cycle systems by using part load approach to ensure the system part load capacity to meet the system load in a time step. In order to simulate real system performance, on/off stage in a time step is required. Since it reduces iteration when a system cycles, it is expected to reduce execution time. 

## E-mail and  Conference Call Conclusions ##

N/A

## Overview ##

When a cycling system is called, the corresponding system module calculates its full capacity and compares the system load. When the system load is less than the system full capacity, a par load performance is assumed and part load penalty is applied in a time step, no matter how small a time step is. In addition, iteration is needed to calculate part load ratio, so that the part load capacity meets the system load. In general, a real system would not be on/off several times during several minutes. Instead, the real system turns on in several minutes, and turns off in several minutes. Therefore, EnergyPlus does not simulate real cycling system performance. 

## Approach ##

The proposed approach adds a new optional field to allow users to select on/off stage in a time step for cycling systems, so that real system performance could be simulated and execution time could be reduced.  

This capability is requested as a SOEP HVAC feature.

### Revise ZoneControl:Thermostat ###

An optional field as the last field will be added to allow systems to operate either OnOff switch or normal thermostat operation 

	  A12 ; \field OnOff Thermal Control Flag
       \type choice
       \key Yes
       \key No
       \default No
       \note Select Yes to force system on in a whole time step.
       \note Select No to disable this feature.

### Revise GetZoneAirSetPoints ###

The function of GetZoneAirSetPoints in the ZoneTempPredictorCorrector will be revised to add a reading section to read an additional field of the ZoneControl:Thermostat and to set up the OnOffControlFlag based on input.

### Revise CalcPredictedSystemLoad ###
  
When the OnOffControlFlag is true, the value of ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired is set to very high in the CalcPredictedSystemLoad function of the ZoneTempPredictorCorrector module, so that the value is much larger than any possible capacity of systems to force system to operate in a whole time step when HVAC system is requested.

### Revise UpdateSystemOutputRequired ###

If priority is greater than 1, the ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired is set to zero, so that any HVAC systems with priority > 1 will not turn on.

If necessary, the values of two variables of ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP and ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP will be set to zero for any HVAC systems with priority > 1.
   
### Calculation procedure ###

The control logic for OnOff thermostat is to set TotalOutputRequired with very high value in the predictor for a system. The rest of systems serving this zone will get zero values of the RemainingOutputRequired or RemainingOutputReqToCoolSP and RemainingOutputReqToHeatSP. Therefore, it only allows a single system or equipment to turn on with priority = 1. 

## Testing/Validation/Data Sources ##

Multiple tests will be performed to ensure a single system only turn on in a whole time step.

## Input Output Reference Documentation ##

\subsection{ZoneControl:Thermostat}\label{zonecontrolthermostat}

The thermostatic zone control object is used to control a zone to a specified temperature. ZoneControl:Thermostat references a control type schedule and one or more control type objects which in turn reference one or more setpoint schedules. The example at the end of this section illustrates a complete zone thermostat specification including the control type and setpoint schedules.

The control type schedule and the list of control type/name pairs are directly related. The schedule defines the type of control that is to be used throughout the simulation. Valid Control Types are:

0 - Uncontrolled (No specification or default)

1 - Single Heating Setpoint

2 - Single Cooling SetPoint

3 - Single Heating/Cooling Setpoint

4 - Dual Setpoint (Heating and Cooling) with deadband

Thus, if the schedule referenced in the ZoneControl:Thermostat statement has a control type of 4 for a particular time period, this indicates that during that time period ``Dual Setpoint with deadband'' control is to be used. The specific ``Dual Setpoint (Heating and Cooling) with deadband'' control object to be used is specified in the list of control type/name pairs. Then the specific control type objects reference the thermostat setpoint temperature schedule to be used. Because only one control can be specified for each control type in a ZoneControl:Thermostat statement, there are only four pairs possible in a particular ZoneControl:Thermostat type/name list. This is because individual controls can be defined throughout the simulation, thus giving the user a full range of flexibility. Since putting in the name of the control type directly in the schedule would be very cumbersome, the control types are assigned a number which is used in the schedule profile.

\subsubsection{Inputs}\label{inputs-054}

\paragraph{Field: Name}\label{field-name-052}

Unique identifying name for the thermostat.

\paragraph{Field: Zone or ZoneList Name}\label{field-zone-or-zonelist-name-001}

Name of the zone or set of zones that is being controlled. When the ZoneList option is used then this thermostat definition is applied to each of the zones in the zone list effecting a global definition for thermostatic control in the zone.

\paragraph{Field: Control Type Schedule Name}\label{field-control-type-schedule-name}

Schedule which defines what type of control is active during the simulation. Valid Control Types are:

0 - Uncontrolled (No specification or default)

1 - Single Heating Setpoint

2 - Single Cooling SetPoint

3 - Single Heating Cooling Setpoint

4 - Dual Setpoint with Deadband (Heating and Cooling)

Each non-zero control type which is used in this schedule must appear in the following fields which list the specific thermostat control objects to be used for this zone.

\paragraph{Field Set (Control Object Type, Control Name)}\label{field-set-control-object-type-control-name}

Up to four pairs of Control Object Type and Control Name may be listed to specify which control objects are used for this zone. This list is not order-dependent, and the position in this list has no impact on the control type schedule. In the control type schedule, a value of 1 always means ``Single Heating Setpoint'', even if that control type is not first in this list.

\paragraph{Field: Control Object \textless{}x\textgreater{}Type}\label{field-control-object-xtype}

This field specifies the control type to be used for this zone. Available control types are:

ThermostatSetpoint:SingleHeating

ThermostatSetpoint:SingleCooling

ThermostatSetpoint:SingleHeatingOrCooling

ThermostatSetpoint:DualSetpoint

\paragraph{Field: Control \textless{}x\textgreater{} Name}\label{field-control-x-name}

The corresponding control type name. The name is used in an object with the name of the control type and specifies the schedule.

<span style="color:red;">**Field: OnOff Thermal Control Flag}\label{field-onoff-thermal-control-flag}**<span>

<span style="color:red;">This optional choice field provides a choice for users to determine if a HVAC system serving this zone turns on in a whole time step or not. Valid choices are Yes and No. If Yes is selected, the system will operate in a whole time step, regardless zone load. The priority of the system defined in the ZoneHVAC:EquipmentList object should be 1. Any other priorities will turn off. If No is selected, this option is disabled and the system will operate based on zone load. The default value is No.  

An example of this statement in an IDF is:

\begin{lstlisting}

ZoneControl:Thermostat, Zone 3 Thermostat, NORTH ZONE,
  Zone Control Type Sched,
  DUAL SETPOINT WITH DEADBAND, VAV Setpoints;
\end{lstlisting}

## Input Description ##

An optional field will be added as the last field in the ZoneControl:Thermostat object, so that no transition is needed. Any new additions will be highlighted in red.

	ZoneControl:Thermostat,
   	\memo Define the Thermostat settings for a zone or list of zones.
   	\memo If you use a ZoneList in the Zone or ZoneList name field then this definition applies
   	\memo to all the zones in the ZoneList.
  	A1 , \field Name
       \required-field
       \reference ZoneControlThermostaticNames
  	A2 , \field Zone or ZoneList Name
       \required-field
       \type object-list
       \object-list ZoneAndZoneListNames
  	A3 , \field Control Type Schedule Name
       \note This schedule contains appropriate control types for thermostat.
       \note Control types are integers: 0 - Uncontrolled (floating, no thermostat), 1 = ThermostatSetpoint:SingleHeating,
       \note 2 = ThermostatSetpoint:SingleCooling, 3 = ThermostatSetpoint:SingleHeatingOrCooling, 4 = ThermostatSetpoint:DualSetpoint
       \required-field
       \type object-list
       \object-list ScheduleNames
  	A4 , \field Control 1 Object Type
       \required-field
       \type choice
       \key ThermostatSetpoint:SingleHeating
       \key ThermostatSetpoint:SingleCooling
       \key ThermostatSetpoint:SingleHeatingOrCooling
       \key ThermostatSetpoint:DualSetpoint
  	A5 , \field Control 1 Name
       \note Control names are names of individual control objects (e.g. ThermostatSetpoint:SingleHeating)
       \note Schedule values in these objects list actual setpoint temperatures for the control types
       \required-field
       \type object-list
       \object-list ControlTypeNames
  	A6 , \field Control 2 Object Type
       \type choice
       \key ThermostatSetpoint:SingleHeating
       \key ThermostatSetpoint:SingleCooling
       \key ThermostatSetpoint:SingleHeatingOrCooling
       \key ThermostatSetpoint:DualSetpoint
  	A7 , \field Control 2 Name
       \note Control names are names of individual control objects (e.g. ThermostatSetpoint:SingleHeating)
       \note Schedule values in these objects list actual setpoint temperatures for the control types
       \type object-list
       \object-list ControlTypeNames
  	A8 , \field Control 3 Object Type
       \type choice
       \key ThermostatSetpoint:SingleHeating
       \key ThermostatSetpoint:SingleCooling
       \key ThermostatSetpoint:SingleHeatingOrCooling
       \key ThermostatSetpoint:DualSetpoint
  	A9 , \field Control 3 Name
       \note Control names are names of individual control objects (e.g. ThermostatSetpoint:SingleHeating)
       \note Schedule values in these objects list actual setpoint temperatures for the control types
       \type object-list
       \object-list ControlTypeNames
  	A10, \field Control 4 Object Type
       \type choice
       \key ThermostatSetpoint:SingleHeating
       \key ThermostatSetpoint:SingleCooling
       \key ThermostatSetpoint:SingleHeatingOrCooling
       \key ThermostatSetpoint:DualSetpoint
  	A11, \field Control 4 Name
       \note Control names are names of individual control objects (e.g. ThermostatSetpoint:SingleHeating)
       \note Schedule values in these objects list actual setpoint temperatures for the control types
       \type object-list
       \object-list ControlTypeNames

<span style="color:red;">**	A12;  \field OnOff Thermal Control Flag **

       \type choice
       \key Yes
       \key No
       \default No
       \note Select Yes to force system on in a whole time step.
       \note Select No to disable this feature.


## Outputs Description ##

insert text

## Engineering Reference ##

insert text

## Example File and Transition Changes ##

One of existing example files will be modified to use this option.

No transition is needed, since an optional field is added as the last one.


## References ##

A link to the enhancement item is provided below.
 
https://github.com/NREL/EnergyPlusDevSupport/blob/master/DesignDocuments/EnhancementList/HVAC_General_2013_05.doc


